# αD — Validated / Invalidated / Demoted / Still-Open Ledger (SK-V17 → SK-V18)

**Pass:** PASS-ALPHA cycle **V5**, bracketing SK-V18 (the GENERALIZATION cycle).

**FOLD (V4 CHALLENGE dispositions resolved):** the V4 CHALLENGE wave
(`research/alpha-hardening/V4/`) disposed αD **ACCEPT on every section across all
seven lenses — ZERO REVISE, ZERO REJECT against αD.** CH1 §αD ("the V3 REVISE is
resolved; αD is internally consistent and well-cited. ACCEPT"), CH2 §4 (αD tally
**ACCEPT ×4 / REVISE ×0**), CH3 (αD ACCEPT — "αD is the *ledger* artefact"), CH4
(F14 confirmed a **no-op** for αD's cost rows), CH5 §4 (**αD tally ACCEPT ×4 /
REVISE ×0 / REJECT ×0**: D.1 phantom-`G` test-only, D.2 G4 `G`-axis-only, D.3
pre-block, D.4 the 18→14 fold all ACCEPT), CH6 (αD rows ACCEPT), and CH7 §4
(**"αD overall: ACCEPT (all sections)"** — the V3 lone REVISE folded; I7 is a
*claim-row* disposing to the crate-wide P1, **not a close-gate definition**) all
converge: **there is NO orphan REVISE or REJECT against αD to resolve.** The V3
lone REVISE (the stale checkasm "18") was already folded clean in the V4 artefact
(§1 V4 now carries the disk-true **14**; §8.V4 R1 records it).

Per the orchestrator's zero-orphan + cohort-concordance discipline, this V5
revision therefore folds the **one non-blocking cohort sharpening** the V4 lenses
canonicalized — the **crate-wide SECOND x86 surface** (CH7 §4 / αC §6 FOLD-1):
`bbnf-simd/ext/x86/` = **3499 LOC** vendored x264/FFmpeg `cglobal`/AVX-512-ZMM
`.asm` (3554 incl `LICENSE-VENDOR`) + a **102-LOC `build.rs`** nasm-rs x86-assembler
driver + the `nasm-rs="0.3"` build-dep + the `src/lib.rs:247` "Contract documented
in `ext/x86/bbnf.asm`" reference — a second x86 footprint BESIDE `src/x86_64/`
(742 `.rs`-LOC / 24 files). CH7 §4 (`:194`) ACCEPTED αD precisely **because** αD's
I7 disposition already routes to "DELETE the entire `src/x86_64/` tree …" and αD's
§8/SYNTHESIS-reference already carry the crate-wide scope, so αD carries **no
false close-gate** — but αD's I7/S1 ROW TEXT still enumerated only `src/x86_64/`,
while the rest of the cohort (αC §6 FOLD-1 — the canonical crate-wide fold;
SYNTHESIS `:491`; HANDOFF; αE P1) names BOTH surfaces. V5 brings αD's I7 reality +
S1 owner-surface + close-gate into concordance with αC §6 FOLD-1 so a downstream
S-P3 P1-enumeration reading αD cannot under-scope the deletion set or false-green a
`find …/src/x86_64 -type f = 0` close-gate while ~3.5K LOC of x86 ASM + an
x86-assembler build driver survive. **No measurement, disposition, or finding of
αD is reversed** — the x86 tree IS overfit and IS P1's target across BOTH surfaces;
only αD's enumeration of WHAT x86 surface exists and its close-gate are widened.
Recorded in §8.V5 + §6. **Zero orphan REVISE remains against αD.**

**FOLD (V3 CHALLENGE dispositions resolved):** the V3 CHALLENGE wave
(`research/alpha-hardening/V3/`) disposed αD **ACCEPT on every lens EXCEPT one
single REVISE** — CH1 §αD, CH4 (αD cost rows), CH7 §4, and the V3 CONSOLIDATED all
converge on the **identical, isolated** defect: the §1 **V4** VALIDATED row carried
a **stale checkasm harness count "18"** (`alphaD-validated-invalidated.md:85`), while
the disk truth — re-verified live at HEAD `318d9c046` (`ls bbnf-simd/tests/checkasm_*.rs
| wc -l` → **14** = 12 single-kernel differentials + `checkasm_common.rs` + `checkasm_parity.rs`)
— and **every other cohort artefact** (αA §3.4, αC §2.6, αE F4, SYNTHESIS, HANDOFF)
already carry the corrected **14**. αD §1 V4 was "the lone surviving '18' in the cohort"
(CH7 §4, CONSOLIDATED). This was **not cosmetic**: αD §1 calls V4 the gold standard for
the G6 same-wave-consumer rule, and a downstream S-P3 gate keyed to "18 present" would
be **un-satisfiable on a clean tree — the very P4-class false-gate this cycle deletes**.
CH2 §4 (αD tally ACCEPT ×4 / REVISE ×0), CH3 (αD ACCEPT), CH5 §D (ACCEPT all), and CH6
(αD rows ACCEPT) found **no other αD defect**. This V4 revision therefore: (a) corrects
the §1 V4 row to the disk-true **14** with the live HEAD command + cross-artefact
concordance; (b) records the fold in §8; and (c) reverses **no** measurement, disposition,
or path:line — αD's substance was otherwise correct across V3. **Zero orphan REVISE
remains against αD.**

**FOLD (V2 CHALLENGE dispositions resolved):** the V2 CHALLENGE wave
(`research/alpha-hardening/V2/`) disposed αD **ACCEPT across every lens** — CH1
§"αD" ACCEPT, CH2 §4 αD tally **ACCEPT ×4 / REVISE ×0**, CH3 αD ACCEPT, CH5 §D all
sub-sections ACCEPT (I5 two-axis FOLDED, S9 DocumentView citation FOLDED, DM2
Sheets FOLDED), CH6 αD-touching rows ACCEPT, CH7 §4 αD overall ACCEPT (I1–I10 each
attach to a re-verified surface). **There is NO orphan REVISE or REJECT against αD
to resolve.** Per the orchestrator's V3 zero-orphan discipline, this V3 revision
therefore folds the three NON-blocking *sharpenings* the V2 lenses surfaced (each
strengthens, not reverses, αD), re-anchors the entry HEAD, and re-verifies every
load-bearing claim live at the V2 bracket HEAD:

1. **I5 phantom-`G` precision** (CH7 §4 / CH5 §D.2) — the V2 fold established the
   two-axis caveat (`K` real, `G` phantom). CH7 sharpens it: the **only** non-default
   `G` instantiations in the tree live in `tape/event_grammar_tests.rs`
   (`_proof_compiles::<JsonEventGrammar>` `:18`, `_proof_compiles::<SheetsEventGrammar>`
   `:20`, and `pub fn leak() -> ValueRef<…, JsonEventGrammar>` `:89`) — a **test-coverage
   surface**, ZERO production-path `G` instantiations. The phantom claim is thus tighter
   than "defaulted to `AnyGrammar` everywhere": it is "defaulted in all production; the
   sole real `G` use is the compile-proof test." Folded into I5, S9, §6.
2. **CSS fact-stream RETIRED, not just diagnostic** (CH3 §"pre-block" / CH7) — re-verify
   at HEAD: `emit_fact_stream` appears ONLY in a retired-marker comment
   (`runtime_generator.rs:686`) and three NEGATIVE-assertion gates
   (`codegen/lib.rs:551,568,578` `assert!(!…contains("emit_fact_stream"))`). The CSS
   fact-stream `String`-as-output plane is GONE, not merely demoted. The G2 retirement
   of `CSS_GENERATED_RS` routes toward grammar lowering and must NOT re-introduce a
   fact-stream carrier. Folded into §5 pre-block, §6.
3. **`CssEventGrammar` does NOT exist at HEAD** (CH2 §5 / CH5 / CH7) — verified: only
   `JsonEventGrammar` + `SheetsEventGrammar` witnesses exist; no `CssEventGrammar`. G4's
   INSTANTIATE branch for the CSS side has no extant type — it is burden-of-proof
   creation, not a rename. Folded into S9, DM2.

**V2 fold (carried, re-verified):** the three V1→V2 sharpenings remain correct at
HEAD — (a) `ValueRef` two type params `K=AnyKind` (REAL) / `G:EventGrammar=AnyGrammar`
(PHANTOM); G4 targets `G` ONLY (`K` must not be conflated/deleted); (b) `DocumentView`
SOLE production impl is `grammars/json/view.rs:68` (`tape/mod.rs:227` is the trait/assoc
def); (c) the REAL Sheets source is `grammar/google-sheets/google-sheets.bbnf` (185-LOC
Pratt operator-precedence formula grammar).

**Subject of SK-V18:** the inflection-point backtrack — ONE grammar-driven
generator emitting JSON + CSS + Sheets from `.bbnf`, over the unified
tape/`ValueRef` substrate, with a shared value-API trait, PRESERVING the >SOTA
proven on hand-written/forked parsers in SK-V17.
**Entry HEAD:** `318d9c046` (`docs(sk-v18-handoff): generalization-cycle path-forward
seeded from V3 audit` — the V2-fold + handoff committed HEAD). SK-V17 closed at
`f6a38445b`; V3 audit committed at `7dbe44c22`.
**Seed:** `restart/prompts/SK-V18-GENERALIZATION-HANDOFF.md` +
`restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` + `V3/AGENT-{1..6}-*.md`.
**Method:** every row cites a commit SHA, a RESULTS.md row, or a verified
`path:line`. Verified-direct claims were re-grepped at HEAD `318d9c046` (see §6
verification log). This is NOT a re-open of any pre-blocked REDRESS family (§5).

---

## §0 — Headline

We stand **exactly on the inflection point** the user defined: JSON and CSS are
both >SOTA with a working value API, on a genuinely unified tape/`ValueRef`
substrate (Lock 1 holds) — **but the parsers are hand-written and FORKED, not
grammar-derived.** The load-bearing WINS that carry into SK-V18 are the
*substrate*, the *measurement discipline*, and the *speed/parity facts*. The
INVALIDATED items are the *codegen-as-grammar-driven* claim, the *7-grammar
count*, the *NEON-hot-path* claim, the *phantom `ValueRef<G>`*, the *x86 tree*
(crate-wide: BOTH `src/x86_64/` AND the `ext/x86/` vendored ASM + nasm build
driver — V5 fold), and the *old contrived CSS bench*. The still-open items ARE the SK-V18
generalization candidates (G1–G6 + PROVE-Sheets + H1).

The whole point of SK-V18: **the hand-written parsers become byte-for-byte
parity oracles, not the product.** A grammar-derived parser that loses the speed
or the equality is not done (HANDOFF §0).

---

## §1 — VALIDATED (load-bearing; carry forward into SK-V18 as foundation)

These are the SK-V17 wins that are real, measurement-honest, and are the
substrate the generalization rides. **Do NOT re-prove; preserve.**

| # | Validated item | Evidence (SHA / RESULTS / path:line) | Why it carries |
|---|---|---|---|
| V1 | **Substrate-union (Lock 1 holds).** ONE `Tape`/`ValueRef`/`PayloadArena`/`TapeBuilder`; both JSON+CSS ride it; CSS at-rule tag reuses the sparse `flag_cursors`/`flag_values` pair — no second tape. | `tape/mod.rs:38,94,175`; CSS reuse `css_l4_*/generated.rs:8-11`, `tape/mod.rs:96-98,144`; AGENT-6 §SUBSTRATE-UNION; landed `1c5bd7a25` (`feat(sk-v16-W6-tape)`) | This is the genuine, generalizable foundation. SK-V18 emits accessors OVER this tape; introducing a second `StructLayout`/`TapeStructBuilder`/`TapeCursor` is a Lock 1 violation (REJECT, pre-blocked §5). |
| V2 | **JSON >SOTA (unconditional `parse_only` plane).** Cold per-parse, strict per-iter equality vs independent `sonic_rs`/`serde_json`; Track 1 > sonic Skipper on every corpus. | `skinny/RESULTS.md` (twitter parse_only 8349 vs 4913 Mbps; canada 16709 vs 12970, +45.4% vs simdjson DOM; marine_ik 9505 vs 5338 ≈ +78%; apache_builds thinnest +1.4%); strict equality `gate.rs:2395,2472,2554,2941-2952`; AGENT-1 §JSON >SOTA Validity; AGENT-5 §6 | The unconditional, generalizable >SOTA proof. SK-V18's grammar-derived JSON parser (G1) must preserve THIS, measured the same way. |
| V3 | **CSS rich-summary >SOTA (canonical harness).** N=200 cold median, real corpus (71KB–495KB), per-corpus distinct medians, no broadcast, independent cssparser 9-field oracle, no `target-cpu=native`. | `css_canon_bench.rs:146-176,250`; `w2_rich_cssom_bench.rs`; final ratios bootstrap 2.210× · animate 2.355× · tailwind 3.348× · material 1.996× (`f6a38445b` HANDOFF close §); A5 live-reproduced 2.145/2.905/1.911/1.975×; AGENT-5 §1,§3,§5 | The CSS >SOTA headline IS measurement-valid. SK-V18's grammar-derived CSS recognizer (G2) must preserve it — but framed honestly (see still-open S8/H1). |
| V4 | **NEON checkasm discipline (kernels that HAVE NEON bodies).** **14 checkasm files = 12 single-kernel differentials + `checkasm_common.rs` (trampoline) + `checkasm_parity.rs` (aggregate)** (NOT 18 — an "18-present" gate is un-satisfiable on a clean tree; this is the P4-class false-gate seed this cycle deletes); scalar-ref-as-executable-spec; alignment/set-size/adversarial sweeps; SIGSEGV/SIGBUS/SIGILL trampoline; stack-canary clobber detection; callee-saved sentinel verify; grammar-neutral byte-set kernels. | verified `ls bbnf-simd/tests/checkasm_*.rs \| wc -l` → **14** (12 single-kernel + common + parity) at HEAD `318d9c046`; `checkasm_common.rs:50,85`; AGENT-4 §5; AGENT-2 F6; matches αA §3.4 / αC §2.6 / αE F4 / SYNTHESIS / HANDOFF | The kernel-validation methodology is the gold standard. SK-V18 G6 (wire-or-retire + ASM backlog) lands new kernels through THIS discipline (same-wave consumer). |
| V5 | **`bbnf-simd` alphabet kernel is genuinely grammar-neutral (caller-data).** Kernels take `set:&[u8]`/`opens/closes:&[u8]`; `dispatch.rs` selects on `lo6_table_admissible(alphabet)` data predicate, not grammar name; CSS rides the neutral `runtime_simd.rs` bridge ("delimiter policy comes from the generated grammar module, never hardcoded"). | `bbnf-simd/src/lib.rs:20,106`; `dispatch.rs` `SelectedBackend{Scalar,NeonTbl4}`; `runtime_simd.rs:1-10,17-20`; AGENT-4 §2 census (clean ✓); AGENT-6 F5; AGENT-2 F6 | The SIMD substrate IS already generalized. SK-V18 G5 migrates JSON's bespoke scanner ONTO this neutral kernel (JSON is the legacy holdout). |
| V6 | **Canonical bench harness is honest (cold / N≥50 gate-enforced / no broadcast / independent oracle).** The SK-V16 contrivance family (24-row broadcast, brace-counter, fixture byte-equality short-circuit, single-sample, fake @generated) is ABSENT from the SK-V17 *measured* CSS path. | `css_canon_bench.rs:250` (`assert!(n >= 50)`); per-corpus distinct ROWs `:261-277`; AGENT-5 §1,§3,§4 (all CLEAN); broadcast killed per `f6a38445b` | This is the measurement plane SK-V18 keeps (H2). The OLD path (`nonjson_css_l4.rs measure_mbps`) is INVALIDATED separately (I6) and deleted by P2. |
| V7 | **Regen discipline honest.** All per-grammar runtime files carry true `crate::GENERATED_HEADER` headers; round-trip clean (`git diff HEAD` empty on generated files); `validate_unique_targets`/`validate_generated_roster` enforce roster integrity; `regen --check` 9/9 exit 0 at close. | `runtime_generator.rs:121,174`; `regen.rs:21-42,90`; AGENT-3 §3 (PASS); `f6a38445b` HANDOFF W5 close | The regen PLUMBING is load-bearing and correct. The defect is upstream (WHAT the generator emits, not the round-trip). SK-V18 keeps the plumbing, replaces the const-string bodies. |
| V8 | **FNV closed-enum + per-corpus typed schema remain bench-quarantined.** `parse_string_enum`/`YStringUnicode`/`fingerprint` only in `bbnf-bench`; the only `fnv64` in `runtime/` is a diagnostic `input_fnv64=` provenance stamp, gating no parse work. | `bbnf-bench/src/{generated_real_typed,real_typed_struct,direct_struct}.rs`; CSS stamp `css_l4_*/generated.rs:393-394,899-900`; AGENT-1 F6; AGENT-5 §4 (CLEAN); CH1 V2 re-verified the `fnv64` is the `push_hex64(&mut out, fnv64(...))` diagnostic stamp at `css_l4_*/generated.rs:394/899` (no parse-gating role) | The quarantine holds. SK-V18 must NOT migrate FNV to runtime (pre-blocked §5). |

---

## §2 — INVALIDATED (claims corrected; these ARE the SK-V18 work)

These are SK-V17 close claims (or implicit framings) that the V3 audit
**corrects**. Each is the trigger for a named SK-V18 generalization/prune wave.

| # | Invalidated claim | Reality (path:line / SHA, verified) | SK-V18 disposition |
|---|---|---|---|
| I1 | **"CSS is grammar-driven / `@generated`."** | `CSS_GENERATED_RS` is a hand-authored Rust `const &str` recursive-descent scanner emitted verbatim; the `.bbnf` is NEVER consumed by the CSS emit path (`emit_request_facts` feeds only config constants). `@generated` header is true-but-empty. **Identical to the SK-V16 finding, UN-REMEDIATED.** | `runtime_generator.rs:701` (verified at HEAD: `const CSS_GENERATED_RS: &str = r#"`); emit `:91`; config-only `:105-134`; AGENT-2 F1, AGENT-6 F1, AGENT-3 §2a | **G2** — retire `CSS_GENERATED_RS`, route CSS through grammar lowering; hand-written becomes parity oracle. LOW risk (hot path scalar; no fragile kernel). |
| I2 | **"JSON codegen projects the parser from the grammar."** | `json_sink_direct::render` consults `SinkOnlyProgram` only to `validate()` presence-of-shapes, then `out.push_str(r#"<fixed rust>"#)`. The byte-dispatch, recursion, scanners, `JsonValue` model are baked literals. A different grammar would `validate()`-fail or emit JSON anyway. | `json_sink_direct.rs:4-16,124-249` (`match byte {…}` at `:138-164`); validate-only `:18-66`; AGENT-1 F1, AGENT-6 F2, AGENT-3 §2b | **G1** — make `json_sink_direct::render` PROJECT from the program; hand-written template = byte-for-byte parity oracle; >SOTA preserved. |
| I3 | **"7 CSS sub-grammars admitted."** | The 7 `css_l4_*/generated.rs` are **byte-identical** (verified at HEAD: md5 `at_rules_and_media` ≡ `visual_functions` = `b654562ccff46ed62dd48e9ace325830`); `parser.rs`/`sink.rs` likewise 0 diff. Only `config.rs` differs (4 diagnostic-identity lines). Root cause: all 7 `RuntimeTarget`s share `CSS_L4_ROOTS=["…/stylesheet.bbnf"]` + `entry_rule:"stylesheet"`. ONE parser replicated 7×. | verified `md5 …generated.rs` 7→1 distinct; `regen_css.rs:5-22,41-43`; AGENT-3 §1a; CH4 V2 `wc -l`=6370 (≈910 ea) | **P3** — collapse to ONE CSS grammar (or differentiate roots so output genuinely diverges). New CHALLENGE lens: distinct-grammar-output (N grammars = N non-identical `generated.rs`). |
| I4 | **"One codegen path."** | The generator is FORKED: `RuntimeEmitterKind = {CompiledLowering(JSON), RequestFacts(CSS)}` — a de-facto grammar-family fork wearing abstract clothing (`CompiledLowering`→JSON templates, `RequestFacts`→CSS const-string courier). The courier variant CANNOT emit a different grammar. | `grammar_provider.rs:40` (verified at HEAD: `pub enum RuntimeEmitterKind`); dispatch `runtime_generator.rs:16-26`; AGENT-3 §2, AGENT-4 §F1,§7 | **G3** — un-fork into ONE grammar-agnostic, program-driven emitter; retire `RuntimeEmitterKind`. New CHALLENGE lens: single-emitter-path. |
| I5 | **"`ValueRef<G:EventGrammar>` grammar-parametric projection" (W2 ledger).** | **Two-axis caveat (V2 fold) + test-only-`G` precision (V3 fold, CH7 §4):** `ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>` has TWO type params. The `K` (Kind) axis is **REAL and load-bearing** — instantiated `ObjectKind`/`ArrayKind`/`StringKind`/`NumberKind`/`BoolKind`/`NullKind` (`json/view.rs:86,143,197,222,244,256`). The `G` (EventGrammar) axis is the **PHANTOM**: defaulted to `AnyGrammar` in **all production code**; the ONLY non-default `G` instantiations in the entire tree are the compile-proof tests in `tape/event_grammar_tests.rs` (`_proof_compiles::<JsonEventGrammar>` `:18`, `::<SheetsEventGrammar>` `:20`, `pub fn leak() -> ValueRef<…, JsonEventGrammar>` `:89`). `EventGrammar`'s methods have ZERO non-test call sites; the witnesses are inert. The W2 "grammar-parametric projection" claim is **not load-bearing** (the projection rides `K`, not `G`). The value API is DIVERGENT (JSON recursive tree + visitor + `DocumentView`; CSS flat token stream, no visitor, not `DocumentView`) — no shared `Value`/`Document`/`Cursor` trait. | verified `tape/mod.rs:175` (`K = AnyKind, G: EventGrammar = AnyGrammar`); `K` real `json/view.rs:86,143,197,222,244,256`; sole `G` uses are test-file `event_grammar_tests.rs:18,20,89`; AGENT-6 F3,F4; CH7 §4 ("only `_proof_compiles` in `_tests.rs`") | **G4** — shared `Value`/`Document`/`Cursor` trait both instantiate; INSTANTIATE-OR-DELETE the phantom **`<G>` (the EventGrammar axis ONLY)** — `K` is real, must NOT be conflated or removed (abrogate-before-patch). The INSTANTIATE branch must CREATE `CssEventGrammar` (does NOT exist at HEAD) — burden-of-proof, not a rename. New CHALLENGE lens: phantom-generic (the test-only `G` use must NOT count as a real instantiation). |
| I6 | **(W3 close)** "NEON structural-index acceleration of the CSS scan." | The hot scan `find_component_delim` is **purely scalar**. `find_css_significant`/`find_comment_close` are **dead at admission** (callers `lib.rs:574,598,608` are inside `#[cfg(test)] mod tests` at `lib.rs:51-52`); only `count_top_level_commas` reaches a generated module (`generated.rs:810`, ×7), and only in the COLD rich-summary projection. **2-of-3 CSS NEON consumers dead; the third is cold.** The hot CSS scan is scalar; the >SOTA does NOT currently depend on NEON. | scalar `runtime_generator.rs:1357-1380`; dead callers `lib.rs:574,598,608` (inside `#[cfg(test)]`); cold reach `generated.rs:810`; AGENT-2 F3, CONSOLIDATED C1, CH7 §0 verify; W3 commit `6bb4b2a6c` | **G6** — wire-or-retire honestly into the hot path; correct the ledger. (Good news: no fragile hand-tuned kernel to preserve → low backtrack risk.) New CHALLENGE lens: acceleration-wiring (kernel reached AT ADMISSION, not `#[cfg(test)]`). |
| I7 | **(implicit)** aarch64-only mandate satisfied. | **TWO x86 surfaces (V5 fold, CH7 §4 / αC §6 FOLD-1 crate-wide):** (a) `bbnf-simd/src/x86_64/` = **742 `.rs`-LOC**, **24 files** (23 `.rs` + 1 `.asm`; CH7 §0 census), declared **unconditionally** at `lib.rs:5` (NOT `#[cfg(target_arch=…)]`-gated), 0 real x86 intrinsics, 14 `unimplemented!("Wave 6")` stubs; AND (b) `bbnf-simd/ext/x86/` = **3499 LOC** vendored x264/FFmpeg `cglobal`/AVX-512-ZMM `.asm` (`bbnf.asm` 485 + `x86util.asm` 1036 + `x86inc.asm` 1978; 3554 incl `LICENSE-VENDOR`), assembled by a **102-LOC `build.rs`** nasm-rs x86-assembler driver (`Cargo.toml:8 build="build.rs"`, `:19 nasm-rs="0.3"`), referenced at `src/lib.rs:247` ("Contract documented in `ext/x86/bbnf.asm`"). Pure wrong-arch scaffolding on BOTH surfaces. | verified at HEAD: `find …/src/x86_64 -type f \| wc -l` → 24, `… -name '*.rs' \| xargs wc -l` → 742; `find …/ext/x86 -type f \| xargs wc -l` → 3554 (3499 `.asm`); `wc -l …/build.rs` → 102; `grep -niE 'nasm\|build *=' Cargo.toml` → `:8`,`:19`; `lib.rs:247`; `lib.rs:5,285-287`; AGENT-4 F3,§4; CH7 §4 / αC §6 FOLD-1 | **P1** — DELETE **BOTH** x86 surfaces crate-wide: `src/x86_64/` (24 files / 742 `.rs`-LOC) + the unconditional `pub mod` + the dead call site + AND `ext/x86/` (3499 LOC ASM) + the `build.rs` nasm driver + the `nasm-rs` build-dep + the `lib.rs:247` ASM-contract reference + the `lock14_baseline` x86 entries. (LOC label per CH4: `−742 .rs` for `src/`; ~3.5K ASM for `ext/`; close gate is crate-wide, NOT `src/`-scoped — αC §6 FOLD-1.) |
| I8 | **(implicit)** "Lock-14 zero-leak gate is green and meaningful." | The gate PASSES (`accepts_current_allowlist` 2/0) **only because** `GENERIC_SCAN_ROOTS` deliberately OMITS `runtime_generator.rs` + `json_sink_direct.rs` + `json_typed_direct.rs` + `json_templates/`; the leak files route into a weaker `SKV15_W2_EXTRA_COVERAGE_ROOTS` check that never runs the neutrality scan; x86 tree tagged `"diagnostic-x86"`. **A green gate over standing leaks.** | `lock14_baseline.rs:2409` (`GENERIC_SCAN_ROOTS`), `:2442,2456,2463`; AGENT-4 F4,F5,§3; CONSOLIDATED D4 | **P4** — extend `GENERIC_SCAN_ROOTS` to cover the leak surface + `FORBIDDEN_GENERIC_TOKENS` with `CSS_`/`_RS`/`EventGrammar`/`*EventGrammar` patterns; a green gate must be meaningful. New CHALLENGE lens: gate-scope-honesty. |
| I9 | **(implicit)** the CSS >SOTA is equal-work CSSOM-vs-CSSOM. | `track1_rich` *counts* 9 aggregate fields lazily (zero payload writes, value-HEAD classification) while lightningcss *builds an owned typed CSSOM* — NOT equal work. The rich rider does cost ~25–33% over the 4-field path (real per-node work), so "materially less severe" than a brace-counter, but the honest framing is **"lazy rich-summary beats eager full-CSSOM,"** not "equal-work CSSOM beats CSSOM." | `generated.rs:305-331`; live `track1_4field` 3106.6 vs `track1_rich` 2329.8 Mbps; AGENT-5 §2,F3; CONSOLIDATED C2 | **H1** — re-frame as lazy-rich-summary vs eager-full-CSSOM, OR add a symmetric materialization-depth comparator (lightningcss tokenize-only). The canonical harness is honest; keep it. |
| I10 | **(metalang leak)** clean shipped runtime symbols. | Bench wave-id `parse_w11_1_number_*`/`_object_direct`/`_array_direct` is baked into the SHIPPED production `runtime/.../json/generated.rs` (**7 occurrences**, verified at HEAD). A SK-V14 bench-wave tag is now a permanent production symbol. Violates `clean-regen-discipline`/`no-metalanguage-docs`. | verified `grep -c parse_w11_1 …/json/generated.rs` → 7; source `json_sink_direct.rs:147,187,227,…`; AGENT-1 F3 | **P5** — rename `parse_w11_1_number_*` → `parse_number_*` in `json_sink_direct.rs`, re-regen. Add grep gate: shipped runtime has no `w[0-9]+` / corpus-name / `sk_v` tags. |

---

## §3 — DEMOTED (validated-but-conditional; not load-bearing as headline)

| # | Item | Status | Note |
|---|---|---|---|
| DM1 | **JSON typed `direct_to_struct`/`real_typed_struct` >SOTA rows.** | VALID-but-CONDITIONAL | Margins (citm +57%, canada +74%) are headline but ride the per-corpus bench-only typed schema (`xtask/real_typed_schema.rs`, 1014-LOC hand fn w/ per-corpus capacity literals). A fair speed comparison (sonic deserializes into the same struct) but conditional on hand-tuning that does NOT generalize. The **unconditional** generalizable proof is `parse_only` (V2). AGENT-1 F7,§JSON >SOTA caveat. SK-V18: keep bench-side; do not claim as grammar-general; the consolidated narrative MUST distinguish the two planes. |
| DM2 | **"Substrate/tape/NEON model generalizes to Sheets/BBNF-self/math/csv."** | DEMOTED to "substrate-READY, not proven" | The substrate (content-agnostic tape, caller-data alphabet, sparse-flag branch tags) PLAUSIBLY generalizes; the value-API/codegen demonstrably do NOT (I1–I5). `sheets_witness/` is a ~25-line `EventGrammar` stub (`event_grammar_witness.rs:4 SheetsEventGrammar`, `mod.rs`) — type-level witness only, no runtime/value-API/scanner. AGENT-6 F6, AGENT-3 §4. **A REAL Sheets grammar EXISTS** — `grammar/google-sheets/google-sheets.bbnf` (185 LOC, Pratt operator-precedence formula grammar) — so the PROVE-wave has a genuine source, NOT a fabricated one (V2 fold, CH2 §4.3). **Note (V3 fold): `CssEventGrammar` does NOT exist at HEAD** (only `Json`+`Sheets` witnesses) — G4's CSS-side INSTANTIATE is creation, not rename. SK-V18 PROVE-wave de-stubs Sheets FROM that `.bbnf` via the generator ONLY — the honest third-grammar litmus. Its Pratt/operator-precedence shape is the generality stress (no JSON/CSS rule needs Pratt). |
| DM3 | **5 aarch64 "neon" primitives wired as the NEON kernel set.** | DEMOTED — scalar passthroughs | `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_class_from_table_64`, `eob_pad_clamp` `_neon` bodies call the scalar reference (0 intrinsics). Not a contrivance (scalar is the spec) but the `_neon` suffix overstates. `dispatch.rs:66-74`; `aarch64/*.rs:3-5`; AGENT-4 F6. SK-V18 G6: give real NEON bodies (PMULL prefix-XOR first) or mark honestly. New CHALLENGE lens: `_neon`/`_simd`-suffix truth. |
| DM4 | **`digit_mac` UDOT kernel.** | DEMOTED — orphan | Real inline `udot` asm (`aarch64/digit_mac.rs:40`) referenced ONLY by its own test (`tests/aarch64_primitives.rs:170`) — never wired into any number parser. A working dotprod asset unused. AGENT-4 F7. SK-V18 G6 ASM backlog: wire into JSON/CSS number scanner or delete (same-wave consumer rule). |

---

## §4 — STILL-OPEN (the SK-V18 generalization candidates)

These are the inflection-backtrack items. They map 1:1 to the SK-V18 §3 backlog
(HANDOFF) and to the INVALIDATED triggers above. Each names the file surface and
the parity oracle that proves it preserves the >SOTA.

| # | Open item (SK-V18 candidate) | Trigger | Owner surface | Parity oracle (preserve >SOTA) |
|---|---|---|---|---|
| S1 | **P1: DELETE BOTH x86 surfaces crate-wide** (`src/x86_64/` 742 `.rs`-LOC / 24 files AND `ext/x86/` 3499 LOC vendored ASM + the nasm build driver). | I7 | `bbnf-simd/src/x86_64/`, `lib.rs:5,285-287`, `lock14_baseline.rs` x86 entries, **AND `bbnf-simd/ext/x86/` (3 `.asm` + LICENSE), `build.rs` (102-LOC nasm-rs driver), `Cargo.toml:8,19` (`build="build.rs"` + `nasm-rs` dep), `src/lib.rs:247` (ASM-contract ref)** | build green on aarch64; gate no longer tags `diagnostic-x86`; **crate-wide close gate (αC §6 FOLD-1 / SYNTHESIS `:491`):** `find …/src/x86_64 …/ext/x86 -type f` → 0 AND `grep -riE 'avx\|gfni\|sve\|x86\|nasm' bbnf-simd/` returns only aarch64-neutral comments — NOT a `src/`-scoped `find …/src/x86_64 -type f = 0`. |
| S2 | **P2: DELETE old contrived CSS bench** (`measure_mbps`/`lightningcss_facts` warm 85–357B SHA-fixtures, more-work-competitor). | I6-adjacent / AGENT-2 F4 | `nonjson_css_l4.rs:528-544,3091-3114` + fixture SHA scaffolding `:59-203,1988+,2502` | canonical `css_canon_bench` remains the sole honest harness (V6). |
| S3 | **P3: COLLAPSE 7 CSS replicas → 1.** | I3 | `regen_css.rs:5-22,41-43`; the 6 redundant `css_l4_*/` dirs | distinct-grammar-output gate: surviving `generated.rs` set has no byte-identical pair (md5-distinct co-gate). |
| S4 | **P4: FIX Lock-14 gate exclusion holes.** | I8 | `lock14_baseline.rs:2409` `GENERIC_SCAN_ROOTS` + `FORBIDDEN_GENERIC_TOKENS` (add `EventGrammar`/`*EventGrammar` per CH3 fold) | `accepts_current_allowlist` green AFTER scan-roots cover the leak surface — a meaningful green. |
| S5 | **P5: PURGE `parse_w11_1_*` metalang leak.** | I10 | `json_sink_direct.rs:147,187,227,…` → re-regen `json/generated.rs` | grep gate: no `w[0-9]+`/corpus-name/`sk_v` in shipped runtime; regen --check clean. |
| S6 | **G1: JSON parser PROJECTED from grammar.** | I2 | `json_sink_direct::render` + `SinkOnlyProgram` | the current hand-written template = byte-for-byte parity oracle; JSON `parse_only` >sonic preserved (V2). |
| S7 | **G2: CSS grammar LOWERING; retire `CSS_GENERATED_RS`.** | I1 | `runtime_generator.rs:701`; route via `emit_from_source`/successor | 9-field `assert_rich_strict_equality` on real corpus + CSS >SOTA preserved (V3), honestly framed. LOW risk (hot path scalar). MUST NOT re-introduce a fact-stream carrier (RETIRED, §5). |
| S8 | **G3: UN-FORK generator** (retire `RuntimeEmitterKind`). | I4 | `grammar_provider.rs:40`; `runtime_generator.rs:16-26,91-94` | one program-driven `render()` emits JSON OR CSS from the lowered program alone. single-emitter-path gate (zero `match grammar`/`RuntimeEmitterKind` family arms). |
| S9 | **G4: shared `Value`/`Document`/`Cursor` trait; INSTANTIATE-OR-DELETE phantom `<G>` (EventGrammar axis only).** | I5 | trait def `tape/mod.rs:175` (`ValueRef` two-axis decl) + `tape/mod.rs:227` (`DocumentView` TRAIT/assoc def); **divergent impl surface G4 generalizes: `grammars/json/view.rs:68` (`impl DocumentView for JsonDocument` — the SOLE impl; CSS has none)**; `event_grammar.rs:31` (inert `EventGrammar`); **the ONLY real `G` uses are `event_grammar_tests.rs:18,20,89` (test-coverage; do NOT count as production instantiation)** | both JSON+CSS instantiate one trait family (mirroring the `json/view.rs` impl on the CSS side); the phantom **`G`** axis either gains a real **production** non-default instantiation (CREATE `CssEventGrammar` — absent at HEAD) or is removed — **the `K` (Kind) axis stays (already real)**. phantom-generic gate (test-only `G` use is NOT a pass). |
| S10 | **G5: migrate JSON scanner onto neutral NEON kernel.** | AGENT-6 F5 | `json/scan.rs:201-…` (bespoke string-aware) → `bbnf-simd` alphabet kernel + `runtime_simd.rs` bridge | JSON >SOTA preserved with both grammars' scanners as alphabet=caller-data instances of one kernel (V5). |
| S11 | **G6: wire-or-retire CSS NEON into hot path + aarch64 ASM backlog** (PMULL prefix-XOR, UDOT `digit_mac`, TBX `byte_class_from_table_64`, CSSC `eob_pad_clamp`, FMOV/CNT `bulk_emit_positions`, + 5 scalar passthroughs). | I6 / DM3 / DM4 | `runtime_simd.rs:29,112,169`; `dispatch.rs:66-74`; `aarch64/{digit_mac,bitmap_prefix_xor_64,…}.rs` | each kernel reached AT ADMISSION (acceleration-wiring gate: ≥1 non-`cfg(test)` caller) + checkasm differential PASS (V4 discipline) + same-wave consumer. Retire gated on samply non-top-N. |
| S12 | **PROVE: Sheets → real grammar via the generator ONLY.** | DM2 | **Source: `grammar/google-sheets/google-sheets.bbnf` (185-LOC Pratt operator-precedence formula grammar — a REAL grammar EXISTS in tree)**; target `sheets_witness/` (~25-line stub) | a third grammar's value-API + projection + scanner fall out of the generator with ZERO hand-written `_GENERATED_RS` block; non-identical `generated.rs`. **Generality STRESS:** the Sheets grammar's Pratt/operator-precedence tower (`:92`), `error_literal` (`#N/A`/`#REF!`/`#DIV/0!`, `:34-37`), and `cell_ref`/`cell_or_range` (`:62-84`) shapes exercise generality NO JSON/CSS rule needs (neither uses Pratt). **Honest-finding candidate:** if the `SinkOnlyProgram`/`BackendShape` lowering cannot express Pratt precedence, that is a genuine §6-style finding — surface it (the Pratt lowering becomes a named, validated, grammar-parameterized primitive), do not paper-close. The honest generalization litmus. |
| S13 | **H1: honest CSS >SOTA framing** (lazy-rich-summary vs eager-full-CSSOM OR symmetric comparator). | I9 | `nonjson_css_l4.rs`/RESULTS rows narrative; optional lightningcss tokenize-only comparator `css_canon_bench.rs:118-121,282-403` | the column/narrative names the materialization-depth asymmetry; the canonical harness stays (V6/H2). |

**Carry-forward soft spots (not waves, but narrative obligations):** DM1 (typed
rows conditional on per-corpus schema — distinguish from unconditional
`parse_only`); the typed-schema bench path must become JSON-Schema-ingested, not
a 1014-LOC hand fn, before typed rows claim grammar-generality (AGENT-1 PR-3).

---

## §5 — PRE-BLOCKED (do NOT re-open; carried verbatim from the seed)

SK-V18 candidates must NOT re-open these REDRESS families (HANDOFF §5 invariant 6
+ SK-V17 HANDOFF Pre-Blocked Routes). αD asserts NONE of S1–S13 re-opens any:

- **AZ-IV eager-value-tree** materialization (118× regression); eager per-leaf
  payload / f64-alloc-per-number / per-color `Box<CssColor>`. Materialization
  stays lazy-by-default via `ValueRef`. (G4's shared trait is over the EXISTING
  lazy `ValueRef`; it does NOT eager-materialize.)
- **StructRegistry / Arena<G> / Builder<G>** hot-path indirection (28–65×; 983×
  css bootstrap; 10583× tailwind WATCHDOG). No registry in the per-leaf hot path.
  (G4 is a trait abstraction, not a registry/indirection.)
- **CSS fact-stream String** serialization as a live admission plane
  (`emit_fact_stream`/`CSS_GENERATED_RS`/`CssFullParseSummary`): **RETIRED, not
  merely diagnostic** — `emit_fact_stream` exists at HEAD ONLY in a retired-marker
  comment (`runtime_generator.rs:686`) + three negative-assertion gates
  (`codegen/lib.rs:551,568,578` `assert!(!…contains("emit_fact_stream"))`) (V3 fold,
  CH3). G2 retires the const string TOWARD grammar lowering, NOT toward a
  re-introduced fact-stream carrier — a fact-stream re-land is a REJECT.
- **`W5C_REQUEST_FACT_PROFILES`** hand-coded CSS profile array: already RETIRED at
  SK-V17 W1 (`codegen/lib.rs:298` retired-marker); do not relocate into projection
  data (the overfit re-entry seam).
- **24-row broadcast** (one timing tuple → 24 conceptual admits): killed at
  SK-V17 W0; the N≥50 telemetry retired it.
- **Fixture / FNV contrivances; FNV production migration** — FNV stays bench-only
  (V8). (P5 purges a SYMBOL-NAME leak, not an FNV migration.)
- **x86 / AVX-512 / SVE** (Apple cores have no SVE). aarch64 NEON + optional
  dotprod/i8mm only. (P1 DELETES x86 — enforces, does not re-open. **V5 fold: P1
  deletes BOTH x86 surfaces crate-wide — `src/x86_64/` AND `ext/x86/` (3499 LOC
  vendored ASM) + the `build.rs` nasm-rs driver + the `nasm-rs` build-dep + the
  `lib.rs:247` ASM-contract ref — per αC §6 FOLD-1; re-introducing EITHER surface,
  or a nasm/x86-`.asm` build path, is a REJECT.**)
- **No second substrate** — an introduced `StructLayout`/`TapeStructBuilder`/
  `TapeCursor` alongside `Tape`/`ValueRef` is a Lock 1 type-ambivalence violation.
  (S9/G4 emits accessors over the EXISTING tape; no new cursor/builder type. G4
  also must NOT delete the EXISTING real `K` (Kind) axis of `ValueRef` — only the
  phantom `G` (EventGrammar) axis is instantiated-or-deleted; collapsing `K` would
  destroy the typed-view machinery `json/view.rs` rides — V2 fold, CH5 §D.2. The
  test-only `G` uses in `event_grammar_tests.rs` must NOT be relied on as a real
  instantiation to "pass" the phantom gate — V3 fold, CH7 §4.)

---

## §6 — Verification log (direct re-grep at HEAD `318d9c046`)

| Claim | Command | Result |
|---|---|---|
| V2-fold + handoff HEAD | `git log --oneline -1` | `318d9c046 docs(sk-v18-handoff): generalization-cycle path-forward seeded from V3 audit` ✓ |
| SK-V17 close SHA | `git log --oneline -1 f6a38445b` | `docs(sk-v17-w4w5): … SK-V17 CLOSED` ✓ |
| V3 audit SHA | `git log --oneline -1 7dbe44c22` | `audit(skinny-impl-overfit-v3): … SK-V18 = generalization` ✓ |
| x86 tree LOC (surface a) | `find …/x86_64 -name '*.rs' \| xargs wc -l` | **742 total** ✓ (I7); 24 files total (23 `.rs` + 1 `.asm`) |
| **Second x86 surface `ext/x86/` (V5 fold)** | `find …/ext/x86 -type f \| xargs wc -l` | `bbnf.asm` 485 + `x86util.asm` 1036 + `x86inc.asm` 1978 + `LICENSE-VENDOR` 55 = **3554 total (3499 `.asm`)** ✓ (I7 surface b) — vendored x264/FFmpeg `cglobal`/AVX-512-ZMM macro ASM |
| **nasm-rs build driver (V5 fold)** | `wc -l …/build.rs` ; `grep -niE 'nasm\|build *=' Cargo.toml` | `build.rs` **102 LOC** (nasm-rs x86-assembler); `Cargo.toml:8 build="build.rs"`, `:19 nasm-rs="0.3"` ✓ (I7) |
| **`lib.rs:247` ASM-contract ref (V5 fold)** | `sed -n '247p' …/src/lib.rs` | `// Contract documented in ext/x86/bbnf.asm; …` ✓ — a live `src/` reference to the `ext/x86` ASM (P1 deletes it) |
| CSS replica identity | `md5 …at_rules_and_media …visual_functions/generated.rs` | both `b654562ccff46ed62dd48e9ace325830` — **IDENTICAL** ✓ (I3); 7 files → 1 distinct |
| Phantom `<G>` (EventGrammar axis) | `grep ValueRef<…EventGrammar…>` (production) | only `tape/mod.rs:175` decl (`= AnyGrammar`); no production instantiation ✓ (I5) |
| **Sole real `G` is test-only (V3 fold)** | `grep -rn 'EventGrammar\|_proof_compiles' runtime/src \| grep ValueRef\|proof` | `event_grammar_tests.rs:18 _proof_compiles::<JsonEventGrammar>`, `:20 ::<SheetsEventGrammar>`, `:89 pub fn leak() -> ValueRef<…, JsonEventGrammar>` — ALL in `_tests.rs` ✓ (CH7 §4) |
| **`CssEventGrammar` absent (V3 fold)** | `grep -rn 'CssEventGrammar\|struct.*EventGrammar' runtime/src` (non-test) | only `SheetsEventGrammar` (`sheets_witness/event_grammar_witness.rs:4`) + `JsonEventGrammar` (`json/event_grammar_witness.rs:4`); **no `CssEventGrammar`** ✓ (G4 INSTANTIATE = creation) |
| **`K` (Kind) axis is REAL (V2 fold)** | `grep -nE 'ValueRef<.*Kind>' json/view.rs` | `:86 ObjectKind`, `:143 ArrayKind`, `:197 StringKind`, `:222 NumberKind`, `:244 BoolKind`, `:256 NullKind` ✓ — `K` instantiated, must NOT be deleted (CH5 §D.2) |
| **`DocumentView` impl site (V2 fold)** | `grep -rn 'impl.*DocumentView.*for' runtime/src` | SOLE production impl `json/view.rs:68` (`impl<'input> DocumentView<'input> for JsonDocument`) — CSS has none ✓ (CH5 §D.3) |
| **CSS fact-stream RETIRED (V3 fold)** | `grep -rn 'emit_fact_stream' codegen/src runtime/src` | ONLY `runtime_generator.rs:686` (retired comment) + `codegen/lib.rs:551,568,578` (negative-assert gates); 0 live emitters ✓ (CH3) |
| **Sheets `.bbnf` source EXISTS (V2 fold)** | `wc -l grammar/google-sheets/google-sheets.bbnf` | **185 LOC** Pratt grammar; `error_literal :34-37`, precedence tower `:92` ✓ (CH2 §4.3) |
| `CSS_GENERATED_RS` | `grep -n "const CSS_GENERATED_RS" runtime_generator.rs` | `701:const CSS_GENERATED_RS: &str = r#"` ✓ (I1) |
| `RuntimeEmitterKind` | `grep -n "enum RuntimeEmitterKind" grammar_provider.rs` | `40:pub enum RuntimeEmitterKind` ✓ (I4) |
| metalang leak | `grep -c parse_w11_1 json/generated.rs` | **7** ✓ (I10) |
| **checkasm harness count (V4 fold R1)** | `ls bbnf-simd/tests/checkasm_*.rs \| wc -l` ; `… \| grep -vE 'common\|parity' \| wc -l` | **14** total = **12** single-kernel + `checkasm_common.rs` + `checkasm_parity.rs` ✓ — corrects the prior §1 V4 "18"; matches αA §3.4 / αC §2.6 / αE F4 / SYNTHESIS / HANDOFF |

All αD-cited path:line/SHA claims that were independently checkable at HEAD
`318d9c046` resolve as stated. The remaining claims are sourced to the V3 agent
reports (cited per-row) which carry their own path:line evidence.

---

## §7 — Disposition for Pass Alpha (feeds αE shortlist + αF contract)

- **Carry as foundation (§1):** V1–V8. SK-V18 preserves the substrate, the two
  >SOTA proofs (V2 unconditional JSON `parse_only`; V3 canonical CSS rich), the
  checkasm discipline, the neutral alphabet kernel, the honest canonical harness,
  the regen plumbing, the FNV quarantine.
- **The SK-V18 work IS the invalidated set (§2):** I1–I10 each map to a named
  PRUNE/GENERALIZE wave (P1–P5, G1–G6) + H1. PRUNE FIRST (P1–P5), then
  GENERALIZE (G1–G6), then PROVE (Sheets), then HONESTY (H1) — the campaign's
  standing order.
- **Demoted (§3) are narrative obligations + ASM backlog:** DM1 (distinguish
  conditional typed rows from unconditional `parse_only`); DM3/DM4 feed G6.
- **Still-open (§4) = the 13 SK-V18 candidates (S1–S13)** with owner surfaces +
  parity oracles named. Each preserves the >SOTA from the grammar-DERIVED parser
  or it is not done (R10).
- **Success criterion (R10, carry to αF):** all PRUNE+GENERALIZE close; ONE
  generator emits JSON+CSS+Sheets from `.bbnf`; shared value-API trait both
  instantiate; phantom `<G>` instantiated-or-deleted (test-only `G` ≠ pass);
  JSON >sonic AND CSS >lightningcss PRESERVED (cold, real-corpus, honestly
  framed) from grammar-DERIVED parsers; aarch64-only (x86 gone); Lock-14 gate
  meaningful; regen --check clean; PASS-IMPL V4 accepts.

**The inflection arrived at honestly: SK-V18 backtracks the hand-written/forked
parsers into one grammar-driven generator, proves it on a third grammar, and
preserves the >SOTA — not a new-feature cycle.**

---

## §8.V5 — V5 FOLD log (V4 CHALLENGE dispositions resolved)

The V4 CHALLENGE wave (`research/alpha-hardening/V4/`) disposed αD **ACCEPT on
every section across all seven lenses — ZERO REVISE, ZERO REJECT against αD.** The
V3 lone REVISE (stale checkasm "18") was already folded clean in the V4 artefact
(§1 V4 carries the disk-true 14; §8.V4 R1). There is therefore **no orphan
REVISE/REJECT against αD to resolve.** Per zero-orphan + cohort-concordance
discipline, this V5 revision folds the **one non-blocking cohort sharpening** the
V4 lenses canonicalized (the crate-wide second x86 surface), which the rest of the
cohort already carries but αD's I7/S1 row text had not yet enumerated. No prior
disposition is reversed.

| # | V4 disposition / sharpening | Where surfaced | Substance | Re-verify at HEAD `318d9c046` | Folded into |
|---|---|---|---|---|---|
| R1 | **αD ACCEPT ×N on every lens; zero orphan** | CH1 §αD, CH2 §4 (×4/0), CH3, CH4 (F14 no-op), CH5 §4 (×4/0/0), CH6, CH7 §4 ("αD overall: ACCEPT (all sections)") | The seven V4 lenses unanimously ACCEPTED αD. CH7 §4 (`:194`) explicitly ruled αD's I7 a **claim-row** whose disposition routes to the crate-wide P1 (cross-refs CH5 V3) — **not** a close-gate definition — so αD carries no false close-gate; the inventory REVISE landed on αA (the results-extraction INVENTORY), NOT αD. No αD section drew REVISE/REJECT. | the seven `V4/CH{1..7}.md` αD sections | header V5 preamble, this §8.V5 |
| R2 | **Crate-wide SECOND x86 surface — αD I7/S1 row text widened to concordance** (non-blocking cohort sharpening) | CH7 §4 (`:90-117`,`:194`), αC §6 FOLD-1 (the canonical crate-wide fold), SYNTHESIS `:491`, HANDOFF, αE P1 | The V4 lenses canonicalized (from the CH5 V3 §C.5/§F.7 BLOCKING REVISE, originally surfaced against αA) a **second x86 footprint** BESIDE `src/x86_64/`: `bbnf-simd/ext/x86/` = 3499 LOC vendored x264/FFmpeg ASM (3554 incl LICENSE) + a 102-LOC `build.rs` nasm-rs assembler driver + the `nasm-rs="0.3"` build-dep + the `src/lib.rs:247` ASM-contract reference. αC §6 FOLD-1 is the gold-standard crate-wide fold; SYNTHESIS/HANDOFF/αE carry crate-wide scope. αD's I7 DISPOSITION already said "DELETE the entire `src/x86_64/` tree" and αD's §8/SYNTHESIS-reference already carried crate-wide scope (hence CH7's ACCEPT), but αD's I7 reality cell + S1 owner-surface + S1 close-gate ENUMERATED only `src/x86_64/`. V5 widens I7 (both surfaces + build driver + Cargo dep + lib.rs:247), S1 (owner surface + crate-wide close gate), and §5 pre-block — so a downstream S-P3 P1-enumeration reading αD cannot under-scope the deletion set or false-green a `find …/src/x86_64 -type f = 0` gate while ~3.5K LOC of x86 ASM survive. **No finding/measurement of αD reversed** — the x86 tree IS overfit and IS P1's target; only αD's enumeration of WHAT exists + its close-gate are widened. | `find …/ext/x86 -type f \| xargs wc -l` → 3554 (3499 `.asm`); `wc -l …/build.rs` → 102; `grep -niE 'nasm\|build *=' Cargo.toml` → `:8 build="build.rs"`, `:19 nasm-rs="0.3"`; `sed -n '247p' lib.rs` → "Contract documented in ext/x86/bbnf.asm"; `find …/src/x86_64 -type f` → 24; `ls checkasm_*.rs \| wc -l` → 14 (unchanged) | I7 row (widened), S1 row (widened), §5 pre-block x86 bullet, §6 verification log (3 new rows) |

**Net V5 effect:** αD's VALIDATED (§1), INVALIDATED (§2 triggers I1–I6, I8–I10),
DEMOTED (§3), STILL-OPEN (§4 wave map S2–S13), and PRE-BLOCKED (§5 non-x86) sets
are **UNCHANGED in substance** — the V4 CHALLENGE re-confirmed every load-bearing
αD claim ACCEPT. The sole V5 fold (R2) widens αD's I7 reality + S1 owner-surface +
S1 close-gate + §5 x86 pre-block from `src/x86_64/`-only to the crate-wide BOTH-x86
scope already carried by αC §6 FOLD-1 / SYNTHESIS / HANDOFF / αE — bringing αD's
x86 ledger row into full cohort concordance. The checkasm 18→14 (V4 R1),
phantom-`G` test-only (V3 F1), CSS fact-stream RETIRED (V3 F2), and
`CssEventGrammar`-absent (V3 F3) folds remain landed and re-verified. No prior
disposition is reversed; zero orphan REVISE remains against αD.

## §8.V4 — V4 FOLD log (V3 CHALLENGE dispositions resolved)

The V3 CHALLENGE wave (`research/alpha-hardening/V3/`) disposed αD **ACCEPT on every
lens save ONE REVISE**, all six dispositive lenses (CH1, CH7) plus the V3 CONSOLIDATED
converging on the **same isolated** defect. There is exactly one disposition to fold;
no orphan REVISE remains after.

| # | V3 REVISE/sharpening | Where surfaced | Substance | Re-verify at HEAD `318d9c046` | Folded into |
|---|---|---|---|---|---|
| R1 | **§1 V4 row stale checkasm "18" → 14** (the lone REVISE) | CH1 §αD (`:129,167,279`), CH7 §4 (`:134,159,166,311`), V3 CONSOLIDATED | αD §1 V4 asserted "18 differential harnesses" / "`tests/checkasm_*.rs` (18)"; disk truth is **14** = 12 single-kernel differentials + `checkasm_common.rs` (trampoline) + `checkasm_parity.rs` (aggregate). Every OTHER cohort artefact (αA §3.4, αC §2.6, αE F4, SYNTHESIS, HANDOFF) already carried the corrected 14; αD §1 V4 was the lone outlier. Left in a binding feeder ledger, an "18-present" assertion seeds a **P4-class un-satisfiable downstream gate** (the false-gate class this cycle exists to delete) and mis-anchors the G6 same-wave-consumer gold-standard reference. **Mechanical fix; no measurement/disposition/path:line of αD reversed.** | `ls bbnf-simd/tests/checkasm_*.rs \| wc -l` → **14**; `… \| grep -vE 'common\|parity' \| wc -l` → **12**; both ran live at HEAD `318d9c046` | §1 V4 row (corrected), header V4-fold preamble, this §8.V4 |

**Net V4 effect:** αD's VALIDATED (§1), INVALIDATED (§2 triggers I1–I10), DEMOTED (§3),
STILL-OPEN (§4 wave map S1–S13), and PRE-BLOCKED (§5) sets are **UNCHANGED in substance**
— V3 CHALLENGE re-confirmed every load-bearing claim ACCEPT (CH5 §D ACCEPT-all; CH2 §4
αD ACCEPT ×4; CH3 αD ACCEPT; CH6 αD rows ACCEPT). The sole V4 fold corrects the §1 V4
checkasm count `18 → 14`, bringing αD into concordance with the binding αF
contract (SYNTHESIS/HANDOFF already carry 14). The phantom-`G` test-only precision (V3
F1), CSS fact-stream RETIRED (V3 F2), and `CssEventGrammar`-absent (V3 F3) folds remain
landed and re-verified (§6, §8 below). No prior disposition is reversed; zero orphan
REVISE remains.

## §8 — V3 FOLD log (V2 CHALLENGE dispositions resolved)

The V2 CHALLENGE wave (`research/alpha-hardening/V2/`) disposed αD **ACCEPT on
every lens** — there is **zero orphan REVISE or REJECT against αD** to resolve.
The V2 fold (the three V1 REVISEs) was verified landed by CH5 §D ("All seven V1
REVISEs correctly and substantively folded"), CH2 §4 (αD ACCEPT ×4 / REVISE ×0),
CH3, CH6, and CH7 §4 (αD overall ACCEPT). Per V3 zero-orphan discipline, this V3
revision therefore folds the three NON-blocking *sharpenings* the V2 lenses
surfaced (each strengthens αD's claims) and re-anchors the entry HEAD; no
substance is reversed.

| # | V2 sharpening | Where surfaced | Substance | Re-verify at HEAD `318d9c046` | Folded into |
|---|---|---|---|---|---|
| F1 | I5 phantom-`G` is **test-only**, not merely defaulted | CH7 §4 ("only `_proof_compiles` in `_tests.rs`") | The ONLY non-`AnyGrammar` `G` uses are `event_grammar_tests.rs:18,20,89` (`_proof_compiles::<JsonEventGrammar/SheetsEventGrammar>` + `pub fn leak()`); ZERO production instantiations. The phantom claim is tighter: production `G` is uniformly `AnyGrammar`; the sole real `G` use is a compile-proof test. The phantom-generic gate must NOT accept the test-only use as a real instantiation. | `grep` confirms sole `G` uses at `event_grammar_tests.rs:18,20,89` | I5, S9, §5 pre-block, §6 |
| F2 | CSS fact-stream is **RETIRED**, not "diagnostic-only" | CH3 §"pre-block" / CH7 | `emit_fact_stream` exists at HEAD ONLY as a retired-marker comment + three negative-assertion gates; 0 live emitters. G2 must not re-introduce a fact-stream carrier (a re-land = REJECT). | `grep emit_fact_stream` → `runtime_generator.rs:686` comment + `codegen/lib.rs:551,568,578` negative asserts | §5 pre-block, §6 |
| F3 | `CssEventGrammar` **does not exist** at HEAD | CH2 §5 / CH5 / CH7 | G4's INSTANTIATE branch for the CSS side has no extant type — it is burden-of-proof creation, not a rename. Only `Json`+`Sheets` witnesses exist. | `grep` → only `SheetsEventGrammar`/`JsonEventGrammar` witnesses; no `CssEventGrammar` | I5, S9, DM2, §6 |
| F4 | Entry HEAD re-anchored | orchestrator V3 | The V2 doc cited entry HEAD `7dbe44c22`; the V2 CHALLENGE wave re-verified at the now-committed `318d9c046` (V2-fold + handoff). V3 cites `318d9c046` as the bracket HEAD; SK-V17 close `f6a38445b`, V3 audit `7dbe44c22` retained as reference SHAs. | `git log --oneline -1` → `318d9c046` | header, §6 |

**Net V3 effect:** αD's VALIDATED (§1), INVALIDATED (§2 triggers), DEMOTED (§3),
STILL-OPEN (§4 wave map), and PRE-BLOCKED (§5) sets are **UNCHANGED in substance**
— the V2 CHALLENGE confirmed every load-bearing claim ACCEPT. The four V3 folds
(a) tighten the phantom-`G` claim to "test-only, not production," (b) elevate the
CSS fact-stream pre-block from "diagnostic" to "RETIRED — re-land is REJECT,"
(c) record that `CssEventGrammar` must be CREATED (not renamed) for G4's CSS side,
and (d) re-anchor the entry HEAD to `318d9c046` — so the contract that consumes αD
(αF/SYNTHESIS) cannot mis-scope G4 or treat a refuted carrier as merely demoted.
No prior V2 disposition is reversed; no orphan REVISE remains.
