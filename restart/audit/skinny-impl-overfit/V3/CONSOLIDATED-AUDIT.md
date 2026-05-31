# SK-V17 Skinny Implementation Overfit Audit — Consolidated (V3)

Date: 2026-05-31. HEAD: `f6a38445b` (SK-V17 closed). Cycle: V3.
Dispatch: 6 parallel read-only agents (PASS-IMPL-OVERFIT-AUDIT §2).

## Headline

**We are standing EXACTLY ON the inflection point the user defined** — JSON and CSS
are both >SOTA with a working value API — **but the implementation is hand-written
and FORKED, not grammar-generalized. SK-V18 must be the GENERALIZATION cycle:
backtrack the hand-written parsers into ONE grammar-driven generator emitting all
grammars from `.bbnf`, preserving the >SOTA + a unified value API.** This is the
exact "backtrack and generalize at the inflection point" the user named.

The audit also **corrects two SK-V17 close claims** (NEON hot-path wiring; the
rich-vs-lightningcss fairness) — recorded honestly below, not papered over.

| axis | verdict |
|---|---|
| A1 JSON hardcoding | KEEP-LATITUDE (honest hand-craft) + 1 PRUNE (metalang leak); JSON >SOTA VALID; not grammar-derived |
| A2 CSS hardcoding | PRUNE-REQUIRED — hand-written const-string scanner (un-remediated SK-16 finding); NEON unwired from hot path; an OLD contrived bench path exists |
| A3 grammar generality | PRUNE-REQUIRED — 7 byte-identical CSS replicas; FORKED generator; Sheets/BBNF-self stubbed |
| A4 codegen/ASM | PRUNE-REQUIRED — **x86 tree exists (742 LOC, violates aarch64-only)**; template-const Lock-14 leaks; gate papers over by exclusion; NEON discipline strong |
| A5 bench contrivances | SK-V17 canonical >SOTA MEASUREMENT-VALID (cold, real corpus, N≥200, no broadcast) + 1 MEDIUM (lazy-vs-eager) |
| A6 value-API / inflection | **PARTIAL → SK-V18 = GENERALIZATION cycle**; substrate unified (Lock 1 holds) but value-API divergent + ValueRef<G> is a PHANTOM + generator doesn't exist |

## Dispositive findings

### D1 — The "grammar-driven generator" does not exist; it is two forked hand-written parsers (A1/A2/A3/A4/A6, HIGH)
- CSS: `CSS_GENERATED_RS` is a ~646–910-LOC hand-written recursive-descent scanner emitted **verbatim as a Rust `const &str`** (`skinny/crates/codegen/src/runtime_generator.rs:701-1611`); the `.bbnf` grammar is **never consumed** by the CSS emit path (`emit_request_facts` feeds only config constants). This is the **identical SK-V16 finding, UN-REMEDIATED**, now wearing a real `@generated` header (provenance-honest header on hand-written content).
- JSON: `json_sink_direct::render` emits the hot parser as fixed Rust string literals (`:4-16,138-164`); the grammar only `validate()`-gates emission, does not shape it. `json_templates/*.rs` are **byte-identical copies** of the runtime files.
- The generator is FORKED: `RuntimeEmitterKind = {CompiledLowering(JSON), RequestFacts(CSS)}` (`grammar_provider.rs:40`) — a grammar-family fork behind an abstract enum.
- The **7 `css_l4_*/generated.rs` are byte-identical** (`diff` = 0) — ONE CSS parser replicated 7×, materially overstating "7 grammars admitted" (all share `stylesheet.bbnf` / `entry_rule: stylesheet`, `regen_css.rs:23,41`).

### D2 — `ValueRef<G: EventGrammar>` is a PHANTOM generic (A6, HIGH)
Never instantiated with a real grammar (always `AnyGrammar`); `EventGrammar`'s methods have **zero non-test call sites**; `JsonEventGrammar`/`SheetsEventGrammar` are inert witnesses. The W2 "grammar-parametric projection" claim **is not load-bearing**. The value API is DIVERGENT: JSON = recursive document tree (`JsonValue`, `get(key)`, visitor, `DocumentView`); CSS = flat rule/decl/typed-token stream (`CssTypedNode`, no visitor, not `DocumentView`) — they share the `at_cursor` *pattern* hand-copied, **no shared Value/Document/Cursor trait**.

### D3 — An x86 tree exists, violating the aarch64-only mandate (A4, HIGH)
`skinny/crates/bbnf-simd/src/x86_64/` = 742 LOC, 24 files (AVX2/AVX512/GFNI/VNNI/IFMA), declared unconditionally at `lib.rs:5`; contains 0 real x86 intrinsics + 14 `unimplemented!("Wave 6")` stubs. Pure wrong-arch scaffolding. **DELETE.**

### D4 — The Lock-14 gate papers over the template leaks by exclusion (A4, MEDIUM)
`GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) deliberately omits `runtime_generator.rs` (the `JSON_*_RS`/`CSS_*_RS` template consts) + routes the leak files into a weaker check that never runs the neutrality scan; the x86 tree is tagged `"diagnostic-x86"`. **Correction to the audit premise:** `accepts_current_allowlist` PASSES (ran: 2/0) — it is NOT a known failure. A green gate over standing leaks is worse than a red one.

## SK-V17 close claims CORRECTED (honesty)

### C1 — CSS NEON is largely UNWIRED from the hot path (A2, HIGH; corrects my W3 report)
The W3 NEON kernels are checkasm-validated, but `find_css_significant`/`find_comment_close` are **dead at admission** (only `#[cfg(test)]` callers); only `count_top_level_commas` reaches a generated module, in the *cold* rich-summary. **The hot CSS scan is scalar.** The W3 commit title ("NEON structural-index acceleration") overstates what is wired. (The W3 agent flagged the harness reverted its edits twice — the wiring likely did not fully land.) **SK-V18 must wire-or-retire the NEON honestly.**

### C2 — The CSS >SOTA is lazy-count vs eager-materialization (A2/A5, MEDIUM; caveat on my W2/W3 numbers)
The **canonical** harness (`css_canon_bench.rs`, `w2_rich_cssom_bench.rs`) is genuinely cold, real-corpus (71KB–495KB), N≥200, distinct per-corpus medians, no broadcast, genuine independent 9-field cssparser oracle, no `target-cpu=native` dependence (A5 live-reproduced 2.15/2.91/1.91/1.98×). **So the headline numbers are measurement-valid.** BUT: `track1_rich` *counts* 9 aggregate fields **lazily** (zero payload writes, value-head classification) while lightningcss *builds an owned typed CSSOM* — not equal-work. The rich rider does cost ~25–33% over the 4-field path (proving real per-node work), so it is "materially less severe" than a brace-counter, but the honest framing is **"lazy rich-summary beats eager full-CSSOM,"** not "equal-work CSSOM beats CSSOM." SK-V18 should add a symmetric comparator OR state the materialization-depth asymmetry.

### C3 — A SEPARATE, OLD, contrived CSS bench path still exists (A2, HIGH; must be deleted, NOT the source of the headline numbers)
`nonjson_css_l4.rs:528` `lightningcss_facts`/`measure_mbps`: warm (16+2000 iters), times 85–357-byte SHA256-pinned micro-fixtures (not the real corpus), and the timed lightningcss does MORE work (parse + SHA256 + a second cssparser re-parse). This is the SK-16 contrivance family, **still in the tree** — it did NOT produce the W0–W3 headline numbers (those came from `css_canon_bench`), but it is a live contrivance surface and a confusion hazard. **DELETE it in SK-V18.**

## Other findings
- **JSON >SOTA VALID** (A1/A5): cold per-parse, strict per-iter equality vs sonic_rs/serde, Track 1 > sonic +1.4%–78%, no broadcast. The typed-struct rows are conditional on the per-corpus bench schema; `parse_only` is the unconditional proof.
- **FNV closed-enum** (A1/A5): still bench-only, never migrated to runtime. Quarantine holds.
- **Metalang leak** (A1, MEDIUM-PRUNE): bench wave-id `parse_w11_1_number_*` is baked into SHIPPED `runtime/.../json/generated.rs` (7 occurrences) — violates regen discipline.
- **Substrate-union VERIFIED** (A6): one `Tape`/`ValueRef`/`PayloadArena`; both grammars ride it; CSS at-rule tag reuses the sparse flag pair — no second tape. **Lock 1 holds.** This is the genuine, generalizable foundation.
- **NEON discipline strong** (A4): 18 real differential checkasm harnesses, scalar-ref-as-spec, adversarial/corpus sweeps, clobber detection, grammar-neutral byte-set kernels. Caveat: 5 kernels wired as "neon" are scalar passthroughs (`bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_class_from_table_64`, `eob_pad_clamp`); UDOT `digit_mac` orphan — the SK-V18 ASM backlog.

## Inflection-point verdict

**YES — we are at the inflection point** (both JSON+CSS >SOTA + working value API), **which is precisely the trigger to backtrack and generalize, NOT to push further proof.** The substrate (tape/ValueRef/SIMD) generalizes; the value-API + codegen demonstrably do not yet. SK-V18 is the GENERALIZATION cycle.

## SK-V18 actionable backlog (the prune-then-generalize plan)

**PRUNE (course-correct the overfit / contrivance / wrong-arch):**
- P1: DELETE the x86 tree (`bbnf-simd/src/x86_64/`, 742 LOC) — aarch64-only mandate. [D3]
- P2: DELETE the OLD contrived CSS bench path (`nonjson_css_l4.rs measure_mbps`/`lightningcss_facts` warm micro-fixture path). [C3]
- P3: COLLAPSE the 7 byte-identical CSS replicas — one CSS grammar, not 7. [D1]
- P4: FIX the lock14_baseline gate — extend `GENERIC_SCAN_ROOTS` to cover `runtime_generator.rs` + the template files; a green gate must be meaningful. [D4]
- P5: PURGE the metalang bench-wave-id leak (`parse_w11_1_number`) from production JSON runtime. [Other]

**GENERALIZE (the inflection rebuild — backtrack hand-written -> grammar-driven):**
- G1: make `json_sink_direct::render` actually PROJECT the JSON parser from the `SinkOnlyProgram`/grammar (the current hand-written template = byte-for-byte parity oracle). The JSON inflection wave. [A1]
- G2: route CSS through grammar LOWERING — retire `CSS_GENERATED_RS` const string; a grammar-DERIVED CSS recognizer (low risk: the >SOTA does NOT depend on hand-shaping — the hot path is scalar, no fragile kernel to preserve, per A2). The CSS inflection wave. [A2/A3]
- G3: UN-FORK the generator — one grammar-agnostic emitter (retire the `RuntimeEmitterKind` grammar-family fork). [A3/A4]
- G4: a shared `Value`/`Document`/`Cursor` trait both JSON+CSS instantiate (value-API isomorphism); INSTANTIATE-OR-DELETE the phantom `ValueRef<G>`. [D2/A6]
- G5: migrate JSON's scanner onto the neutral alphabet-parametric NEON kernel (JSON is the legacy holdout). [A6]
- G6: WIRE-OR-RETIRE the CSS NEON honestly into the hot path; wire the 5 scalar-passthrough kernels or mark them honestly; the UDOT/PMULL/TBX/CSSC aarch64 backlog. [C1/A4]
- PROVE: bring `sheets_witness` up to a real grammar via the generator ONLY (the honest third-grammar generalization litmus — if one generator can emit a third grammar from `.bbnf`, generalization is real). [A3/A6]

**HONESTY (measurement):**
- H1: re-frame the CSS >SOTA as lazy-rich-summary vs eager-full-CSSOM, OR add a symmetric comparator (materialization-depth parity). [C2]
- H2: the canonical harness (`css_canon_bench`) is the honest one — keep it; delete the OLD path (P2).

## Forward-lens CHALLENGE addenda for SK-V18 S-P0 (proposed by the agents)
- NEW-CH(verbatim-blob): flag const-string-courier emitters (a `@generated` file that is a verbatim `&str` literal in codegen = hand-written, not derived).
- NEW-CH(distinct-grammar-output): N claimed grammars must have N **non-identical** `generated.rs` (diff-census).
- NEW-CH(single-emitter-path): one grammar-agnostic emitter; flag grammar-family forks.
- NEW-CH(phantom-generic): a generic param `<G>` never instantiated with a real type is decorative — flag it.
- NEW-CH(timed-plane-symmetry + corpus-in-the-timer): the >SOTA comparator must do equal work on the real corpus, cold.
- NEW-CH(acceleration-wiring): a "NEON acceleration" claim must show the kernel reached at admission, not only `#[cfg(test)]`.

## Trajectory
SK-V17 proved the substrate + the speed (cold, real-corpus, valid) but on HAND-WRITTEN forked parsers. SK-V18 = the generalization cycle: prune the overfit/x86/contrivance, then backtrack both hand-written parsers into ONE grammar-driven generator over the (already unified) tape/ValueRef substrate, with a unified value API, proven by a third grammar (Sheets) — preserving the >SOTA. This is the inflection the user named, arrived at honestly.
