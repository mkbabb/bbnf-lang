# Handoff SK-V18 — The Generalization Cycle (Inflection Backtrack)

Date: 2026-05-31.
HEAD at bracket: `318d9c046` (SK-V18 generalization handoff committed). SK-V17 closed
at master `f6a38445b` (W5 close `6bb4b2a6c`); the PASS-IMPL V3 audit committed
`7dbe44c22`.
Status: Pass Alpha cycle V5 (alphaF) — FOLDS the V4 seven-lens CHALLENGE dispositions
(`research/alpha-hardening/V4/{CH1..CH7}.md`) per `PASS-ALPHA §3`, atop the V1+V2+V3 folds
(JSON range +1.4%–164.7%; canonical Lock-14 `match grammar`-arm co-gate over codegen AND
xtask; Pratt `google-sheets.bbnf` adoption; `G`-axis-not-`K`-axis phantom resolution;
checkasm count 14; x86 second-surface crate-wide; revert dependency graph + hard-cap
defaults carried). The V4 REVISEs folded here: **(CH6 §1 / CH3 αA-αE / CH1 §αE / CH7 §1 —
the consequential one)** the P1 deletion-target obligation was NARROWER than the crate-wide
verify grep it is gated by — re-grepped at HEAD, the grep ALSO fires on the active `nasm-rs`
Cargo.toml build-dep (`:19`), `lib.rs:5 pub mod x86_64;` + the `#[cfg(target_arch="x86_64")]`
dispatch arms (`:285-288`), and in-crate doc surfaces that the four-item list never named
removing → P1 + `x86_tree_deleted` are EXTENDED so the deletion list is reach-matched to the
grep (a RED-by-construction gate would be the mirror of the V3 escape the fold fixed). This
same fold corrects the αA/αE feeder rows that retained the false-green `src/`-scoped close-gate
after FOLD-1 landed in αC/SYNTHESIS/HANDOFF (orphan-propagation REVISE). **(CH2 §8.1)** the
`runtime_target_rows_collapsed` structural check projected onto only `(source_roots,entry_rule)` —
the two INVARIANT css_l4 columns — discarding the 5 columns (`fact_schema`/`output_plane`/`emitter`/
`row_id` + path) where per-profile divergence demonstrably lives; the projection is WIDENED to
the full per-`grammar_name` config-tuple modulo the generated-artefact path columns (the gate is
correctly RED pre-P3, GREEN only after the 7 profiles genuinely collapse). **(CH6 §13)** the
V3→V4 fold-ledger self-citations (drifted ~50-60 lines) switched to fold-stable section/column
anchors. Folds the binding seed
(`restart/prompts/SK-V18-GENERALIZATION-HANDOFF.md`) + the PASS-IMPL V3 audit
(`restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` + `AGENT-{1..6}-*.md`).

## Benched-substrate disclosure (load-bearing)

This handoff gates the **benched skinny tree** (`skinny/crates/`). The totality tree
(`crates/core/`) is the SK-V19 fold target, NOT an SK-V18 owner path. The unified
substrate is `skinny/crates/runtime/src/tape/` (`Tape:94`/`ValueRef:175`/
`PayloadArena:38`); Lock 1 holds (A6 VERIFIED) — both grammars ride it; the generator
is built ON it, the substrate itself is NOT touched. Every path:line in the SYNTHESIS
was grep-verified at HEAD `318d9c046`.

## Current State (SK-V17 close, the inflection point)

SK-V17 closed at `f6a38445b` with the >SOTA PROVEN on HAND-WRITTEN forked parsers:

- **JSON >sonic-rs VALID** (cold per-parse, strict per-iter equality vs
  sonic_rs/serde, Track 1 +1.4%–164.7%, no broadcast; 51/51 admitted rows; `parse_only`
  the unconditional proof; `+1.4%` = apache_builds thinnest, `+164.7%` =
  unicode_escapes widest, per alphaA §1 / `skinny/RESULTS.md`).
- **CSS rich-typed >lightningcss full-CSSOM** (N=200 cold median, `css_canon_bench`):
  bootstrap 2.210× · animate 2.355× · tailwind 3.348× · material 1.996×; EXACT
  9-field cssparser structural equality; preserve-rich-ast intact (lazy `ValueRef`
  projection, no eager tree).
- **Substrate unified** (Lock 1 holds): one `Tape`/`ValueRef`/`PayloadArena`; both
  grammars ride it — the genuine generalizable foundation.

**We are standing EXACTLY ON the inflection point the user defined** (both JSON+CSS
>SOTA + working value API) — **which is precisely the trigger to backtrack and
generalize, NOT to push further proof.** The implementation is hand-written and
FORKED:

- The grammar-driven generator does NOT exist. CSS = `CSS_GENERATED_RS` hand-written
  recursive-descent scanner emitted verbatim as a `const &str`
  (`codegen/src/runtime_generator.rs`, the `.bbnf` grammar never consumed by the emit
  path — identical UN-REMEDIATED SK-16 finding). JSON =
  `json_sink_direct::render` fixed Rust string literals (`json_sink_direct.rs:4`, the
  grammar only `validate()`-gates). The generator is FORKED:
  `RuntimeEmitterKind = {CompiledLowering(JSON), RequestFacts(CSS)}`
  (`grammar_provider.rs:40-42`).
- The 7 `css_l4_*/generated.rs` are byte-identical (one md5) — ONE CSS parser
  replicated 7×.
- `ValueRef<G: EventGrammar>` is a PHANTOM (test-only `_proof_compiles`,
  `tape/event_grammar_tests.rs:18-21`); the value API is DIVERGENT (JSON tree+visitor
  vs CSS flat stream; no shared Value/Document/Cursor trait).
- An x86 tree exists — TWO surfaces (CH5 V3): `bbnf-simd/src/x86_64/` (24 files, stubs)
  AND `bbnf-simd/ext/x86/` (~3000-LOC vendored ASM) + the nasm `build.rs` driver +
  `src/lib.rs:247` `ext/x86/bbnf.asm` reference. Violates aarch64-only; both must be deleted.
- The Lock-14 gate PASSES by EXCLUSION (`GENERIC_SCAN_ROOTS`,
  `lock14_baseline.rs:2409`, omits `runtime_generator.rs`; x86 tagged
  `"diagnostic-x86"`) — a green gate over standing leaks.
- CSS NEON is unwired at admission (only `count_top_level_commas` reaches a generated
  module, in the cold rich-summary; `find_css_significant`/`find_comment_close` are
  `#[cfg(test)]`-only). JSON has its own bespoke `neon::scan` (`json/scan.rs:201`, not
  neutral). 5 kernels are scalar passthroughs; the UDOT `digit_mac` is an orphan.
- Sheets is a 25-LOC `EventGrammar` stub (no `.bbnf`/parser/`BackendRule`).
- An OLD contrived warm CSS bench still exists (`nonjson_css_l4.rs` `lightningcss_facts`
  + warm `measure_mbps` SHA-fixtures); the `parse_w11_1_number` metalang wave-id leaks
  into the shipped JSON `generated.rs`.

## What SK-V18 Opens

The subject: **backtrack the hand-written, forked, replicated parsers into ONE
grammar-driven generator emitting all grammars from `.bbnf`, over the unified
tape/`ValueRef` substrate, with a unified value-API trait, proven on a third grammar
(Sheets), preserving the >SOTA honestly from the grammar-DERIVED parsers.** The
standing order is **PRUNE first, then GENERALIZE, then PROVE.** Hand-craft was
acceptable to PROVE >SOTA; the hand-written parsers now become byte-for-byte parity
ORACLES, not the product.

The 16 backlog items (V3 §backlog; each carries the V3 finding id):

**PRUNE (delete the overfit / wrong-arch / contrivance — lands FIRST):**
- **P1** DELETE the WHOLE x86 surface crate-wide (CH5 V3 §C.5 + CH6 V4 §1), not just
  `src/x86_64/` — the deletion list is reach-matched to the verify grep (a list narrower
  than the grep ships a RED-by-construction gate, CH6 V4 §1): `bbnf-simd/src/x86_64/` (24
  files, AVX/GFNI/VNNI/IFMA stubs) AND `bbnf-simd/ext/x86/` (~3000-LOC vendored
  `bbnf.asm`/`x86inc.asm`/`x86util.asm`) AND `bbnf-simd/build.rs` (nasm-rs x86-assembler
  driver, delete-or-neutralize) AND re-home `src/lib.rs:247`'s `ext/x86/bbnf.asm` reference
  AND remove the `nasm-rs = "0.3"` build-dep from `Cargo.toml:19` (+`:14-16` comments) AND
  remove `src/lib.rs:5 pub mod x86_64;` + the `#[cfg(target_arch="x86_64")]` dispatch arms
  (`:285-288`) AND scrub the in-crate doc surfaces OR scope the grep to source+manifest AND
  DECOUPLE the COMPILE-COUPLED removal sites the verify grep ALSO fires on (V5 R-2/CH5 §F.6 —
  a deletion list narrower than the grep ships a RED-by-construction gate): `tests/checkasm_parity.rs`
  carries 9 ACTIVE compile-coupled `bbnf_simd::x86_64::…::*_scalar(…)` call sites
  (`:458,:464,:467,:477,:478,:484,:493,:497,:502`) resolving into `src/x86_64/` — DECOUPLE-OR-DELETE
  the x86_64 reference block (and the `#[ignore]` x86 parity harness) so the test crate compiles after
  `src/x86_64/` deletion, retaining the aarch64 parity assertions; AND CLEAN `src/scalar/byte_class_from_eq_set_64.rs`'s
  residual x86 doc strings (`:10,:12,:15` "AVX-512 BW"/"AVX2") to aarch64/scalar-neutral;
  verify crate-wide `grep -riE --include='*.rs' --include='Cargo.toml' 'avx|gfni|sve|x86|nasm'
  skinny/crates/bbnf-simd/` (NOT `src/`-scoped) → aarch64-neutral only, every active hit on
  the removal list (including the 9 `checkasm_parity.rs` `bbnf_simd::x86_64::…` call sites + the
  `byte_class_from_eq_set_64.rs` doc strings); BUILD-SOUNDNESS close-gate: `cargo build` AND
  `cargo test --no-run` clean — the `checkasm_parity.rs` decoupling is what keeps the `src/x86_64/`
  deletion build-sound (without it the test crate fails to compile against the deleted
  `bbnf_simd::x86_64::…` paths). aarch64-only. [D3 + CH6 V4 §1 + V5 R-2/CH5 §F.6]
- **P2** DELETE the OLD contrived CSS bench (`nonjson_css_l4.rs` `lightningcss_facts`/
  warm `measure_mbps` SHA-fixture path); KEEP `css_canon_bench`. [C3]
- **P3** COLLAPSE the 7 byte-identical CSS `generated.rs` → one CSS grammar / N-distinct. [D1]
- **P4** FIX the Lock-14 gate — extend `GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`)
  to cover `runtime_generator.rs` + template files; drop the `diagnostic-x86`
  exclusion; a green gate must be meaningful. [D4]
- **P5** PURGE `parse_w11_1_number` from the shipped JSON `generated.rs`. [Other]

**GENERALIZE (backtrack hand-written → grammar-driven — the inflection):**
- **G1** `json_sink_direct::render` PROJECTS the JSON parser from the
  `SinkOnlyProgram`/grammar; the hand-written template = byte-for-byte parity oracle;
  JSON >sonic-rs preserved. [A1]
- **G2** route CSS through grammar LOWERING — retire `CSS_GENERATED_RS`; a
  grammar-DERIVED recognizer (LOW risk: the >SOTA does NOT depend on hand-shaping —
  the hot path is scalar, no fragile kernel); CSS >lightningcss preserved. [A2/A3]
- **G3** UN-FORK the generator — retire `RuntimeEmitterKind`; one grammar-agnostic
  emitter (SK-V17 residual REDRESS-W2-1 single-emitter lands here). [A3/A4]
- **G4** shared `Value`/`Document`/`Cursor` trait both JSON+CSS instantiate (PRESERVING
  JSON's `get(key)`/typed-`Kind`/visitor through the trait — no LCD flatten);
  INSTANTIATE-OR-DELETE the `G: EventGrammar` axis of `ValueRef` (NOT `K=Kind`, already
  real). DELETE is the abrogate-before-patch default — no `CssEventGrammar` exists, and
  the trait does not require `<G>`. [D2/A6]
- **G5** migrate JSON's bespoke `neon::scan` onto the neutral `select_classifier`
  kernel (JSON is the legacy holdout). [A6]
- **G6** WIRE-OR-RETIRE the CSS NEON honestly into the hot path AT ADMISSION; wire the
  5 scalar-passthrough kernels or mark them honestly; the UDOT `digit_mac`/PMULL/TBX/
  CSSC aarch64 ASM backlog with a same-wave consumer or honest non-top-N measurement. [C1/A4]

**PROVE (the honest generalization litmus):**
- **PROVE** bring `sheets_witness/` (25-LOC stub) up to a REAL third grammar **via the
  generator ONLY** — ADOPT the EXISTING `grammar/google-sheets/google-sheets.bbnf` (a
  genuinely-different Pratt formula grammar; do NOT author a fresh "third JSON" stub that
  hollows the litmus), bring it into the benched skinny tree (new grammar root + xtask
  target), run it through the SAME (G3) generator; the Sheets `generated.rs` md5-distinct
  from JSON+CSS with ZERO hand-authored runtime Rust; the Sheets value type instantiates
  the G4 trait. Pratt-lowering is the generality STRESS + honest-finding candidate. If one
  generator emits a third grammar from `.bbnf`, generalization is REAL; if it cannot via
  the generator ONLY, generalization is NOT real — surface honestly, do NOT stub-prove. [A3/A6]

**HONESTY (measurement):**
- **H1** re-frame the CSS >SOTA as lazy-rich-summary vs eager-full-CSSOM, OR add a
  symmetric materialization-depth comparator; keep `css_canon_bench`; P2 deletes the
  old. [C2]

## Authority

- `restart/prompts/SK-V18-GENERALIZATION-HANDOFF.md` (the binding seed).
- `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` +
  `restart/audit/skinny-impl-overfit/V3/AGENT-{1..6}-*.md`.
- `restart/skinny/tranches/sk-v18/SYNTHESIS.md` (this contract's goalset).
- `restart/skinny/tranches/sk-v18/research/alpha/{alphaA..alphaE}.md` +
  `research/alpha-hardening/V{N}/{CH1..CH6,CONSOLIDATED}.md` (the fold source).
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` + `HANDOFF.md` + `SPEC.md` (the proven
  substrate + the >SOTA harness + the W5 close ledger; SK-V17 residuals routed forward).
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/HANDOFF.md`,
  `restart/locks/LOCKS.md` (Lock 1 substrate-union, Lock 6/14 generated-output, Lock 14
  grammar-neutrality, Lock 16 SIMD parity; 16-lock count = 16).
- `restart/prompts/pass-contracts/PASS-ALPHA.md`, `restart/prompts/ORCHESTRATOR.md`.

## Gate Posture

Per `SK-V18-GENERALIZATION-HANDOFF.md §6`: relinquish only at **G-Alpha and G-Omega**
(the mandatory user gates). Drive the rest. Stop only at a mandatory user gate, an
unrepaired invariant violation, or completed SK-V18 close. (If the active user pin
amends to auto-pass G-Alpha, follow the pin and stop only at G-Omega; the SK-V17
handoff carried that pin — confirm at dispatch.)

Alpha hardening runs the six-lens CHALLENGE pass per `ORCHESTRATOR.md §3W` (CH1
Correctness / CH2 Generality / CH3 Regression / CH4 Cost / CH5 Hidden Coupling / CH6
Next-Tranche-Impact), writing
`restart/skinny/tranches/sk-v18/research/alpha-hardening/V{N}/CH{1..6}.md` +
`CONSOLIDATED.md`. SK-V18 binds the SIX NEW CHALLENGE addenda from the V3 audit (§4 of
the seed) into S-P0 + every pass CHALLENGE:

- **verbatim-blob:** a `@generated` file that is a verbatim `&str` literal in codegen
  is hand-written, NOT derived — REJECT as "grammar-driven."
- **distinct-grammar-output:** N claimed grammars must have N **non-identical**
  `generated.rs` (diff/md5-census) — replicas don't count.
- **single-emitter-path:** one grammar-agnostic emitter; flag grammar-family forks
  (e.g. `RuntimeEmitterKind` JSON-vs-CSS).
- **phantom-generic:** a generic `<G>` never instantiated with a real type outside
  `#[cfg(test)]` is decorative — instantiate-or-delete.
- **timed-plane-symmetry + corpus-in-the-timer:** the >SOTA comparator must do equal
  work on the real corpus, cold (no micro-fixtures, no more-work-competitor).
- **acceleration-wiring:** a "NEON/ASM acceleration" claim must show the kernel reached
  AT ADMISSION (in the hot path), not only under `#[cfg(test)]`.

## Pre-Blocked Routes (binding on S-P0 through S-P3)

SK-V18 must NOT reopen (full semantics in `SYNTHESIS.md §0.4`):

- AZ-IV eager-value-tree materialization (118x); eager per-leaf payload /
  f64-alloc-per-number / per-color `Box<CssColor>`. The shared value-API trait (G4)
  is LAZY over the tape — no eager tree.
- StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65x; 983x css
  bootstrap; 10583x tailwind WATCHDOG). No registry in the per-leaf hot path.
- CSS fact-stream String serialization as a live admission plane
  (`emit_fact_stream`/`CSS_GENERATED_RS`/`CssFullParseSummary`): diagnostic-only. G2
  must NOT replace the const-string courier with a fact-stream String.
- The hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array (retired SK-V17): not
  re-introduced or relocated into projection data (the overfit re-entry seam); every
  residual CSS routing entry names the `.bbnf` rule it derives from.
- The 24-row broadcast measurement (one timing tuple → N conceptual admits).
- Fixture / FNV contrivances; FNV production migration; FNV stays bench-only.
- x86 / AVX-512 / GFNI/VNNI/IFMA / SVE (Apple cores have no SVE), nasm/x86 assembler in
  `build.rs`, the `nasm-rs` build-dep, vendored `ext/x86/` ASM, `lib.rs` x86 module/cfg-arms.
  aarch64 NEON + optional dotprod/i8mm only. P1 enforces by deletion crate-wide, deletion
  list reach-matched to the verify grep (`src/x86_64/` AND `ext/x86/` AND `build.rs` AND
  `nasm-rs` Cargo.toml dep AND `lib.rs` `pub mod x86_64;`/cfg-arms, CH5 V3 + CH6 V4).
- verbatim-blob re-entry (a new const-`&str` courier); phantom-generic re-entry (a
  second uninstantiated `<G>`); distinct-grammar-output violation (byte-identical
  replicas claimed as N grammars).
- brace-counter CSS admission; lightningcss CSSOM comparison before the CSS recognizer
  emits comparable output; deleting legacy generated/runtime shims before replacement
  proof; full-codegen close claims while dirty generated files remain;
  timed-plane-asymmetry / corpus-out-of-timer / more-work-competitor (P2 deletes the
  old; H1 frames the rest).
- No second substrate: an introduced skinny `StructLayout`/`TapeStructBuilder`/
  `TapeCursor` alongside the landed `Tape`/`ValueRef` is a Lock 1 type-ambivalence
  violation (REJECT). G3/G4 emit accessors over the EXISTING `Tape`/`ValueRef`; no new
  cursor/builder type.

Inherited REDRESS families (semantics carried): `28+33, 50-55, 60-72, 80, 82-84, 88,
89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum production migration`. The
SK-V17 residuals (REDRESS-W2-1 single-emitter, crates/core fold, Sheets/BBNF-self
generality) are the SK-V18 SUBJECT (G3/PROVE), admitted to be discharged here — NOT
re-opens.

Hidden-coupling escapes (Lock 1 substrate-union, forbidden unless G-Omega amends):
retained sidecars / sidecar tables / sidecar event vectors, retained cursor/list,
cursor streams, aux density/projection tables, parser-owned structural
projections/streams, parallel source passes, second tapes, public `UnionTape`, new
substrate APIs, a sixth `BackendShape`, production FNV arbiters, production
hash-correctness proof, Track 1 ≡ Track 2 sidecars, wrong-plane comparator admission,
cross-call classifier-state retention.

## Inviolable invariants (verify each cycle close; per seed §5)

1. 16-lock count (`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = 16).
2. 5-shape BackendShape canon; tape = substrate-manifest CATEGORY (not a 6th shape).
3. aarch64-only: zero x86/AVX/SVE/nasm in `bbnf-simd` CRATE-WIDE — `src/x86_64/` AND
   `ext/x86/` AND `build.rs` AND the `nasm-rs` Cargo.toml dep AND `lib.rs`
   `pub mod x86_64;`/cfg-arms all gone (P1 enforces by deletion, deletion list reach-matched
   to the verify grep; verified crate-wide `grep -riE --include='*.rs' --include='Cargo.toml'
   'avx|gfni|sve|x86|nasm' skinny/crates/bbnf-simd/`, NOT `src/`-scoped, CH5 V3 + CH6 V4).
4. Substrate-union (Lock 1): one tape/`ValueRef`; no parallel/second substrate.
5. Grammar-neutral (Lock 14, per the canonical three-surface model `LOCKS.md` item 14:
   `<name>.bbnf` + workspace metadata + optional decl crate; ZERO `match grammar` arms /
   grammar-named modules in generic crates): zero grammar-named branches in generic crates
   (codegen/xtask/bbnf-simd). The gate (P4) must bind THREE canonical surfaces, each
   catching a leak class the others miss: (i) the `GENERIC_SCAN_ROOTS` forbidden-token scan;
   (ii) the canonical `match grammar`-arm grep over the FULL `LOCKS.md:349` alphabet
   `Json|CssL4|(GoogleSheets|Sheets)|Bbnf` across BOTH the codegen AND the xtask
   workspace-metadata surface (`rg -nE 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>' skinny/crates/codegen/src skinny/xtask/src`
   → 0) — `Sheets\w*` does NOT match the canonical `GoogleSheets =>` arm, so `GoogleSheets`
   must be un-abbreviated, and `Bbnf` is carried for SK-V19 forward-safety; the xtask root
   is the canonical surface (b) (`RuntimeTarget`/strategy metadata) where a branch that
   SELF-DISCLOSES a grammar token (`Json =>` etc.) in metadata is caught; (iii) the
   canonical grammar-named-*type* census per `LOCKS.md:349` surface (a)
   (`rg -nE 'JsonParser|CssL4Parser|GoogleSheetsParser|BbnfBootstrap' skinny/crates/codegen/src skinny/xtask/src`
   → 0); (iv) the STRUCTURAL relocated-seam check `runtime_target_rows_collapsed` — all xtask
   `RuntimeTarget` rows sharing one `grammar_name` are byte-identical in EVERY field except the
   generated-artefact path columns (`output_dir`/`expected_files`): `count(distinct
   config-tuple-minus-output_dir) == 1` per `grammar_name` over `fact_schema`/`row_id`/`output_plane`/
   `emitter`/`entry_rule`/`source_roots`/`check_command`/`frontend_requirements` (a
   `(source_roots,entry_rule)`-only `sort -u` is INSUFFICIENT — the live css_l4 divergence rides
   `fact_schema`/`output_plane`/`emitter`, the 5 per-profile columns; correctly RED pre-P3, GREEN
   only post-collapse, CH2 V4 §8.1), because the arm census (ii) is syntactically INCAPABLE of detecting a
   per-grammar branch relocated into a NEUTRAL-identifier data-table — a token-free table has
   no `Json =>` arm syntax for the regex to fire on (CH2 V3 §8.1). They catch different leaks:
   a token scan misses a `match grammar` using neutral identifiers; the arm census misses a
   `CSS_GENERATED_RS` const AND a re-emitted grammar-named type AND a neutral-identifier
   metadata data-table; the row-count check (iv) is the only thing that catches the last. The
   `JsonEventGrammar`/`SheetsEventGrammar` witness surface lives in `runtime/` (not the P4
   scan root) — if the un-forked generator EMITS a grammar-named `EventGrammar` literal, add
   `EventGrammar`/`XEventGrammar` to the `runtime_generator.rs`-scoped forbidden tokens.
6. preserve-rich-ast; no re-opened REDRESS (AZ-IV eager, StructRegistry per-leaf,
   fact-stream-as-output, 24-broadcast, FNV-runtime, x86/AVX/SVE).
7. >SOTA preserved from the grammar-DERIVED parsers (the whole point).

## Next Move

**next-move = run the six-lens CHALLENGE over alphaA-F (binding the six V3 addenda),
converge per ORCHESTRATOR §3Z (≥95% ACCEPT ×2 consecutive, zero orphan REVISE, V≤5),
then present the SK-V18 contract for G-Alpha.** After G-Alpha, dispatch skinny pass
S-P0 (overfit audit with the six addenda) → S-P1 (profile: re-confirm the JSON+CSS hot
leaves on the benched path before any G5/G6 kernel lands, actual-profiling) → S-P2
(research surviving grammar-neutral candidate classes for the generator projection) →
S-P3 (author `sk-v18/SPEC.md` with the §4.4 wave plan + `DISPATCH-PROMPT.md`).

S-P3 sequences the waves **PRUNE → GENERALIZE → PROVE → HONESTY** preserving
dependency order:

1. **PRUNE wave(s):** P1 (delete the WHOLE x86 surface crate-wide, deletion list
   reach-matched to the verify grep — `src/x86_64/` AND `ext/x86/` AND nasm `build.rs` AND
   the `nasm-rs` Cargo.toml dep AND `lib.rs` `pub mod x86_64;`/cfg-arms, CH5 V3 + CH6 V4) ·
   P2 (delete old CSS bench) · P3 (collapse 7
   replicas — AND collapse the 7 xtask `RuntimeTarget` css_l4 rows to one config row, so
   `runtime_target_rows_collapsed` holds, CH2 V3) · P4 (Lock-14 gate meaningful — MUST land
   before the G2/G3 emitter rebuild so the gate is trustworthy) · P5 (purge metalang leak).
   PRUNE first per the standing order.
2. **GENERALIZE wave(s):** G1 (JSON projection, parity oracle) → G2 (CSS grammar
   lowering, retire const-string) → G3 (un-fork the emitter) → G4 (shared value-API
   trait + instantiate-or-delete phantom) → G5 (JSON scanner on neutral NEON) → G6
   (CSS NEON wire-or-retire honestly + ASM backlog). Each primitive lands WITH its
   hot-path consumer in the same commit (no orphan kernels). The >SOTA is re-proven on
   the grammar-DERIVED parser at each generalization that touches the hot path (JSON
   51/51 cold; CSS N≥200 cold median full-CSSOM; EXACT cssparser equality).
3. **PROVE wave:** Sheets `.bbnf` through the generator only; md5-distinct
   `generated.rs`; instantiates the G4 trait.
4. **HONESTY wave:** H1 framing; `regen --check` clean.

S-P3 binds the `--skv18-generalization-report` gate consumer (Section 2 schema:
`grammar_derived`, `parity_oracle_diff`, `verbatim_blob_present == false`,
`emitter_fork_present == false`, `generator_grammar_branch_count == 0` (FULL-alphabet
arm census over codegen AND xtask metadata — self-disclosing-token branches),
`generator_grammar_type_count == 0` (grammar-named-type census),
`runtime_target_rows_collapsed == true` (the STRUCTURAL relocated-seam check the arm
census cannot do — per-`grammar_name` config-tuple collapse over all non-path columns
including the 5 per-profile columns `fact_schema`/`output_plane`/`emitter`/`row_id`/`entry_rule`,
NOT a `(source_roots,entry_rule)`-only projection, CH2 V4 §8.1 / V3 §8.1), `phantom_generic_resolved`,
`shared_value_trait_instantiations >= 2`, `generator_grammar_count == 3`,
`generated_md5_distinct`, `sheets_real_grammar`, `acceleration_at_admission`,
`x86_tree_deleted` (NO x86 surface anywhere in `bbnf-simd` — `src/x86_64/` AND `ext/x86/`
AND nasm `build.rs` AND the `nasm-rs` Cargo.toml dep AND `lib.rs` `pub mod x86_64;`/cfg-arms
gone, deletion list reach-matched to the verify grep, verified crate-wide, CH5 V3 + CH6 V4),
`lock14_gate_scans_codegen`,
`metalang_leak_present == false`,
`materialization_framing`, `corpus_in_timer`, `regen_check_clean`) and re-uses the
SK-V17 JSON + CSS >SOTA guard consumers.

Close criterion (R10): all PRUNE + GENERALIZE waves close; one grammar-driven
generator emits JSON + CSS + Sheets from `.bbnf`; the value API is a shared trait both
instantiate; the phantom `<G>` is instantiated or deleted; JSON >sonic-rs AND CSS
>lightningcss are PRESERVED (cold, real-corpus, honestly framed) from the
grammar-DERIVED parsers; aarch64-only (x86 gone); the Lock-14 gate is meaningful;
regen --check clean. PASS-IMPL V4 (the SK-V18 close audit) accepts every axis or
records intrinsic-block proof with measurement. If a generalization wave proves a
grammar-derived parser CANNOT preserve the >SOTA without hand-shaping, surface it
honestly as a named, validated, grammar-parameterized primitive (not a silent
hand-written blob) — do NOT paper-close (seed §6).

Revert protocol, hard caps, and per-wave triumvirate discipline are sanctioned-deferred
to S-P3 (PASS-ALPHA §4.4 authority), not paper-closed here — with two binding carries so
the deferral is a legitimate handoff, not an uncapped-execution paper-close:
1. **Revert dependency graph:** S-P3's revert protocol MUST encode the entry-gate chain
   PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1 — a wave that fails its exit gate BLOCKS
   every downstream wave that entry-gates on it (no downstream wave dispatches over a
   REDRESSed predecessor; G1 failure blocks G2/G3/G4/PROVE; G3 un-fork failure blocks PROVE).
2. **Hard-cap defaults:** S-P3 MUST carry the standing [dispatch-hard-cap] defaults
   (research/plan/redress 20/15/30 min, "at 0.9N commit, at N halt") unless the wave's risk
   class (the Sheets/NEON cluster is MED-HIGH per alphaE) justifies a documented larger cap —
   so no SK-V18 wave dispatches uncapped.
