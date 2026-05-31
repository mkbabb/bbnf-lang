# AGENT-4 — Codegen/xtask/bbnf-simd Lock-14 leaks + aarch64 ASM/NEON discipline

**Pass:** PASS-IMPL-OVERFIT-AUDIT V3 (closes SK-V17). **HEAD:** `f6a38445b`.
**Axis:** Lock-14 grammar-name leaks in generic infra; aarch64 ASM/NEON discipline; x86 zero-tolerance; the lock14_baseline gate honesty.
**Verdict:** **MIXED, leaning PRUNE-REQUIRED.** The aarch64 NEON kernels are honest, differentially-validated, grammar-neutral work. But the generic codegen crate carries **production grammar-named template constants** that the Lock-14 gate excludes from its own neutrality scan, and a **742-LOC dead x86_64 stub tree** that violates the user's explicit aarch64-only mandate. The "zero-leak" claim in the W3/W5 ledgers is **false as scoped** — it is true only for the subset of files the gate chooses to scan.

---

## 1. Findings (severity + verdict, path:line)

### F1 — [HIGH / PRUNE] Production grammar-named template constants in generic codegen crate
`skinny/crates/codegen/src/runtime_generator.rs` (1611 lines, **no `#[cfg(test)]` — all production**) holds eight grammar-named raw-string template constants:

- `JSON_PARSE_ONLY_GENERATED_RS` (line 195), `JSON_PARSE_ONLY_PARSER_RS` (550), `JSON_MOD_RS` (572), `JSON_HOST_RS` (594)
- `CSS_MOD_RS` (598), `CSS_PARSER_RS` (612), `CSS_SINK_RS` (665), `CSS_GENERATED_RS` (701)

`emit_compiled` (line 29) assembles the JSON runtime from `include_str!("json_templates/...")` + the `JSON_*_RS` consts; `emit_request_facts` (line 76) assembles the CSS runtime from the `CSS_*_RS` consts (lines 91-94). These are two hand-curated, grammar-specific code-template paths baked into the generic codegen crate. **This is the textbook Lock-14 leak** — grammar-name template constants per the AUDIT-4 axis definition. The `RuntimeEmitterKind` enum (below) abstracts the *name* of the fork, but the *bodies* are per-grammar templates.

### F2 — [HIGH / PRUNE] JSON Pattern-H parser emitted as raw strings from `json_sink_direct.rs`
`skinny/crates/codegen/src/json_sink_direct.rs` `render()` (line 4) takes a generic `SinkOnlyProgram` but emits a **fixed hand-written JSON parser** as raw string literals: `pub fn parse_direct<'i, S: JsonSink>` (line 100), `parse_value_direct` (line 128), with JSON-structural byte dispatch hardcoded in the emitted text. The `SinkOnlyProgram` argument barely parameterizes the output — the render functions (`render_string_rule`, `render_number_rules`, `render_value_dispatch`) push constant JSON parser text. This is the JSON analogue of the CSS template path: a Pattern-H hand-parser living inside the generic generator under the `CompiledLowering` label. `mod json_sink_direct;` and `mod json_typed_direct;` are declared at production scope (`lib.rs:4-5`).

### F3 — [HIGH / PRUNE] 742-LOC dead `x86_64/` tree violates aarch64-only mandate
`skinny/crates/bbnf-simd/src/x86_64/` (24 files, 742 LOC) is declared **unconditionally** at `lib.rs:5` (`pub mod x86_64;` — NOT `#[cfg(target_arch = "x86_64")]`-gated). Contents: `avx2/`, `avx512_bitalg/`, `avx512_gfni/`, `avx512_kmask/`, `avx512_vbmi2/`, `avx512_vnni/`, `avx512_vpclmul/`, `avx_ifma/`. **Census:**
- **0** real x86 intrinsics (`_mm256/_mm512/_mm_`) anywhere in the tree.
- **14** `unimplemented!("Wave 6: ...")` bodies (e.g. `avx512_gfni/classify_affine.rs:54`, `avx2/classify.rs:48`).
- The only live call site is `lib.rs:287` behind `#[cfg(all(target_arch="x86_64", target_feature="avx512bw"))]` — dead on this target.

The tree is **elaborate citation-doc scaffolding + scalar reference + stubs**. The user's explicit ask for SK-V18 is "optimize ASM for THIS arch (aarch64), NO x86." This tree is 742 LOC of x86 surface that compiles into the aarch64 build (only because it contains no actual x86 intrinsics) and should be deleted. Only `byte_class_from_eq_set_64.rs` + `.asm` carry a real (avx512bw-gated, unused-on-aarch64) body.

### F4 — [MEDIUM / GATE-HOLE] `runtime_generator.rs` excluded from the Lock-14 neutrality scan
`GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) lists `crates/codegen/src/lib.rs`, `crates/codegen/src/lower`, `crates/codegen/src/grammar_profile.rs` — but **NOT** `crates/codegen/src/runtime_generator.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, or `json_templates/`. The leak files of F1/F2 are routed instead into `SKV15_W2_EXTRA_COVERAGE_ROOTS` (line 2442) — a *weaker, separate* coverage check that does NOT run `validate_generic_source`'s forbidden-token scan. The gate's strict neutrality enforcement (`validate_generic_crate_neutrality`, line 2466 → `validate_generic_source`, line 2693) never sees the worst leaks. This is the same exclusion-hole pattern V1 AUDIT-4 reported, still present.

### F5 — [MEDIUM] `accepts_current_allowlist` PASSES — the gate is green while leaks stand
Contra the audit-prompt expectation of a "known pre-existing test failure," `lock14_baseline::tests::accepts_current_allowlist` **PASSES** at HEAD (`cargo test -p bbnf-bench --lib accepts_current` → `test result: ok. 2 passed; 0 failed`). It passes *because* of F4: `validate(&root)` (line 2731) only neutrality-scans `GENERIC_SCAN_ROOTS`, which omits the leak files. The gate is honest about the files it scans (`lower/` is genuinely clean) but dishonest by omission about the files it declines to scan. **A green gate over a leak is worse than a red one** — it asserts neutrality that does not hold.

### F6 — [LOW / SK-V18 OPP] Five aarch64 "neon" primitives are scalar passthroughs
`dispatch.rs:66-74` wires five primitives as the "NEON" kernel set, but their `_neon` bodies just call the scalar reference (0 NEON intrinsics):
- `bitmap_prefix_xor_64_neon` → scalar (`aarch64/bitmap_prefix_xor_64.rs:3`)
- `bitmap_next_set_bit_neon` → scalar (`bitmap_next_set_bit.rs:3`)
- `bulk_emit_positions_64_neon` → scalar (`bulk_emit_positions_64.rs:3`)
- `byte_class_from_table_64_neon` → scalar (`byte_class_from_table_64.rs:3`)
- `eob_pad_clamp_neon` → scalar (`eob_pad_clamp.rs:5`)

Not a correctness bug (scalar is the spec), and not a *contrivance* per §5 (they don't fabricate a measurement) — but the `_neon` suffix overstates the implementation. These are the prime SK-V18 vectorization targets (see §5).

### F7 — [LOW / SK-V18 OPP] `digit_mac` UDOT kernel is orphan
`aarch64/digit_mac.rs:27` `parse_4_digits_dotprod` (real inline `udot` asm, `digit_mac.rs:40`) is referenced **only by its own test** (`tests/aarch64_primitives.rs:170`) — never wired into any runtime number parser. A working dotprod/UDOT asset sitting unused. SK-V18 should wire it into the JSON/CSS number scanner or delete it.

### F8 — [LOW / DISCIPLINE] Inline `#[cfg(test)]` tests in codegen `src/`
`codegen/src/lib.rs` carries 16 inline `#[cfg(test)] mod`/test blocks (first at line 258), violating the `no-inline-tests` memory rule (all tests in `tests/`). The `CSS_PROFILE_IDS` grammar-name array (lib.rs:304) lives inside this test module, so it is test-only — but its presence is why the leak surface is murky; a reader sees `Css` enumerations in `lib.rs`.

---

## 2. Lock-14 leak census (verifying the zero-leak claim)

| Generic crate / dir | Claim | Reality | Verdict |
|---|---|---|---|
| `codegen/src/lower/` | zero non-comment grammar names | **TRUE** — 1 hit, a log string "JsonSink+DirectBuild" at `lower/sink_only.rs:114` | clean (✓) |
| `codegen/src/runtime_generator.rs` | (excluded from claim) | **8 grammar-named production template consts** (F1) | LEAK |
| `codegen/src/json_sink_direct.rs` | (excluded) | hand-written JSON parser as raw strings (F2) | LEAK |
| `codegen/src/json_typed_direct.rs` | (excluded) | grammar-named typed direct path | LEAK (same class) |
| `codegen/src/json_templates/` | (excluded) | per-grammar template files (`view.rs` 83 hits, etc.) | LEAK (per-grammar-template class) |
| `codegen/src/lib.rs` (production, <line 258) | clean | only `mod json_sink_direct; mod json_typed_direct;` (F8) | borderline |
| `bbnf-simd/src/aarch64/` (kernels) | byte-sets as data | **TRUE** — all kernels take `set:&[u8]`/`opens/closes:&[u8]`; dispatch keys on alphabet data, not grammar name | clean (✓) |
| `bbnf-simd/src/dispatch.rs` | grammar-agnostic | **TRUE** — `SelectedBackend{Scalar,NeonTbl4}`, selects on `lo6_table_admissible(alphabet)` data predicate | clean (✓) |
| `xtask/src/` | (driver layer, not generic infra) | grammar names present (`regen_css.rs`, `skv15_w0.rs`) — legitimate for a CLI driver/regen entrypoint, not Lock-14 generic infra | acceptable |

**Net:** the "bbnf-simd and codegen/src/lower/ have zero non-comment grammar names" claim is **literally TRUE for those two scopes** — but it is a **scope-narrowed claim that elides `runtime_generator.rs` + `json_sink_direct.rs` + `json_typed_direct.rs` + `json_templates/`**, which are the actual leak surface. The ledger told the truth about a deliberately small window.

**`RuntimeEmitterKind` verdict:** the enum (`grammar_provider.rs:40`) is two abstract variants `{CompiledLowering, RequestFacts}` — **not grammar-family-keyed by name.** But `CompiledLowering`→JSON-templates and `RequestFacts`→CSS-templates is a **de-facto grammar fork wearing abstract clothing.** It is a renamed grammar branch in effect, not in identifier.

**`W5C_REQUEST_FACT_PROFILES` verdict:** NOT merely "a retirement comment." It was replaced by `CSS_PROFILE_IDS` (`lib.rs:304`), a live 7-entry array of CSS profile names with a membership `assert!` (line 316). However, this lives inside `#[cfg(test)] mod tests` (line 259), so it is **test-only routing**, not a production grammar-keyed array. Verdict: the retirement is real for production; the grammar-name list survives as a test fixture.

---

## 3. The lock14_baseline gate honesty

- **Why does its test "fail"?** It **does not fail** — `accepts_current_allowlist` PASSES at HEAD (F5). The audit-prompt premise (known failure) is stale or refers to a prior cycle.
- **Is the allowlist honest or papering over leaks?** **Papering over.** The gate's `validate_generic_source` forbidden-token list (`lock14_baseline.rs:2420`) is narrow JSON-identifier-specific (`JsonSink`, `JsonValue`, `JsonNodeKind`, …) and contains **no `CSS_*_RS`, no `Css`, no template-const tokens.** More fundamentally, `GENERIC_SCAN_ROOTS` (line 2409) **excludes the files where the leaks live** (F4). The gate scans a curated subset that is clean, declares neutrality, and routes the dirty files into a weaker `SKV15_W2_EXTRA_COVERAGE_ROOTS` check that does not run the neutrality scan.
- **Does it scan its own exclusions?** No. `SKV15_W2_EXTRA_COVERAGE_ROOTS` is checked for *coverage-report column presence* (`validate_skv15_w2_*`), not for grammar-name neutrality. The x86 tree is explicitly tagged `("crates/bbnf-simd/src/x86_64", "diagnostic-x86")` (line 2463) — the gate *knows* about the x86 tree and labels it "diagnostic" rather than failing on it.

**Gate honesty verdict: COMPROMISED-BY-EXCLUSION.** Mechanically sound for what it scans; dishonest by omission about what it refuses to scan.

---

## 4. x86 audit (must be zero)

| Token | Occurrences in `bbnf-simd/src/x86_64` | Status |
|---|---|---|
| Real x86 intrinsics (`_mm256/_mm512/_mm_`) | **0** | — |
| `unimplemented!()` stub bodies | **14** | dead |
| AVX2/AVX512/GFNI/VNNI/IFMA/VPCLMUL/BITALG/KMASK modules | 7 families, 24 files, 742 LOC | dead scaffolding |
| `pub mod x86_64;` gating | **unconditional** (`lib.rs:5`) | should be deleted, not gated |

**Verdict: FAIL the aarch64-only mandate.** There must be NONE; there are 742 LOC. They compile only because they contain zero real x86 intrinsics — they are citation-doc + scalar-ref + `unimplemented!` placeholders for a "Wave 6" that the SK-V17 close did not deliver. **PRUNE the entire `src/x86_64/` tree** before SK-V18 ASM work; it is pure surface area pointed at the wrong arch.

---

## 5. NEON checkasm discipline

**STRONG and HONEST.** 18 `tests/checkasm_*.rs` differential harnesses. Audited `checkasm_byte_class_from_eq_set_64.rs` + `checkasm_common.rs`:

- **Scalar-reference-first:** every kernel has a scalar reference declared *the executable specification* (e.g. `byte_class_from_eq_set_64_scalar`); candidate (dispatched NEON) is asserted bit-equal.
- **Real differentials:** alignment sweep (0..64 offsets), set-size sweep (1..=8), adversarial seeds (the seeds that caught the `escape_mask_64` bug), empty/constant/duplicate-set edge cases, tail-padding contract, twitter.json corpus parity with splitmix64 rolling digest.
- **Hardening:** SIGSEGV/SIGBUS/SIGILL signal trampoline (`signal_guard`), 1 KiB stack-canary clobber detection (`with_stack_canary_xor_fold`, `checkasm_common.rs:50`), aarch64 callee-saved register sentinel verification (x19-x28, `checkasm_common.rs:85`).
- **Grammar-neutral:** kernels take byte-sets/byte-pairs as data (`set:&[u8]`, `opens/closes:&[u8;SET_CAP]`); no CSS/JSON special-casing. W3 kernels `bracket_depth_mask_64`/`comment_body_mask_64` are wired into runtime (`runtime/src/runtime_simd.rs:47,122`) with open/close bytes passed as data.
- **BBNF_SIMD_STRICT:** **no `BBNF_SIMD_STRICT` env-gate found in the crate** — strictness is enforced structurally (the checkasm tests are unconditional `#[test]`s that panic on divergence), not via an env flag. The `strict` posture in the gate (`SKV15_W2_PRIMITIVE_CLASS_ROOTS` tags aarch64 `"strict-checkasm-admitted"`, line 2456) refers to these tests being the admission gate. Acceptable — the parity tests run on every `cargo test`.

**Caveat (F6):** the checkasm rigor applies to kernels that *have* NEON bodies. Five primitives wired as "neon" in `dispatch.rs` are scalar passthroughs, so their checkasm "differential" is scalar-vs-scalar (trivially passes). Not dishonest, but the parity guarantee is vacuous for those five until they get real NEON bodies.

---

## 6. Underexploited aarch64 instructions (SK-V18 ASM opportunities)

NEON intrinsic census (top): `vdupq_n_u8`×42, `vceqq_u8`×28, `vqtbl4q_u8`×7, `vqtbl1q_u8`×4. **Used well:** TBL/TBX (`vqtbl4q`/`vqtbl1q` in `classify_tbl4.rs`, `unescape_uxxxx.rs`), `udot` (orphan, `digit_mac.rs:40`), `vshrn` narrowing, `vaddv` horizontal-reduce.

**Available-but-unused (the user's "optimize ASM for this arch" forward-lens):**

| Instr | aarch64 feature | Opportunity | Target kernel (F6) |
|---|---|---|---|
| **PMULL / PMULL2** (`vmull_p64`) | `aes`/`pmull` (M-series has it) | carryless-multiply prefix-XOR — the canonical SIMD `bitmap_prefix_xor_64`; replaces the scalar passthrough | `bitmap_prefix_xor_64_neon` |
| **UDOT/SDOT** (`vdotq`) | `dotprod` (M-series has it) | already proven in orphan `digit_mac`; wire into number-span SWAR-replacement and the 8-byte digit MAC | number parsing; `digit_mac` |
| **TBX** (`vqtbx*`) | base NEON | range-extension table lookups beyond TBL4's 64-entry limit — useful for `byte_class_from_table_64` (currently scalar passthrough over a 256-entry table) | `byte_class_from_table_64_neon` |
| **CSSC** (`umin`/`umax`/`abs` scalar) | `cssc` (Armv8.7+; M3/M4-class) | branchless clamp in `eob_pad_clamp` | `eob_pad_clamp_neon` |
| **FMOV/CNT + addv** | base NEON | popcount-driven `bulk_emit_positions` (NEON compaction via `cnt` + prefix-sum) replacing scalar bit-iteration | `bulk_emit_positions_64_neon` |
| **bitmap_next_set_bit** | base | `rbit`+`clz` (already scalar-cheap) — low priority; leave scalar | — |

The five scalar-passthrough kernels (F6) plus the orphan UDOT (F7) are the concrete SK-V18 ASM backlog. **Highest-leverage:** PMULL prefix-XOR (used by JSON string-quote carry and CSS comment/bracket masks — hot path) and wiring UDOT into number parsing.

---

## 7. INFLECTION-POINT: codegen readiness

**NOT READY to be ONE grammar-agnostic generator.** The codegen crate carries **two grammar-family forks** that must be pruned first:

1. The `RuntimeEmitterKind::CompiledLowering` path = JSON hand-template (`json_templates/` + `JSON_*_RS` + `json_sink_direct.rs`'s raw-string JSON parser).
2. The `RuntimeEmitterKind::RequestFacts` path = CSS hand-template (`CSS_*_RS` consts).

The genuinely generic path — `emit_from_source` → `lower/` → `json_sink_direct::render(SinkOnlyProgram)` — exists and `lower/` is clean, but the renderer it feeds (`json_sink_direct.rs`) **emits a fixed JSON parser, not a program-driven one.** Until the SinkOnly renderer is parameterized by the lowered program (so the same `render()` produces JSON *or* CSS output from the `SinkOnlyProgram` alone), the two `RuntimeEmitterKind` variants are grammar forks, not strategies.

**The inflection requires:** (a) delete `src/x86_64/` (F3); (b) collapse `CompiledLowering`/`RequestFacts` into one program-driven emit path, deleting the `CSS_*_RS`/`JSON_*_RS` template constants (F1); (c) make `json_sink_direct::render` actually consume `SinkOnlyProgram` shape rather than push constant text (F2); (d) extend `GENERIC_SCAN_ROOTS` to cover `runtime_generator.rs` et al. so the gate can prove the result (F4).

---

## 8. Prune / course-correct recommendations (concrete)

- **PRUNE-WAVE (x86):** delete `skinny/crates/bbnf-simd/src/x86_64/` entirely + remove `pub mod x86_64;` (`lib.rs:5`) + the `#[cfg(target_arch="x86_64")]` call site (`lib.rs:285-287`) + the `x86_64` entries in `lock14_baseline.rs` (`SKV15_W2_PRIMITIVE_CLASS_ROOTS:2463`, `validate_frozen_status_output` x86 cases). Recovers 742 LOC; satisfies aarch64-only mandate.
- **PRUNE-WAVE (template forks):** collapse `runtime_generator.rs` `emit_compiled`/`emit_request_facts` into a single program-driven emit; delete `JSON_*_RS`/`CSS_*_RS` consts (lines 195-735); make `json_sink_direct::render` (and rename it `sink_direct`) project from `SinkOnlyProgram` rather than emit constant JSON text.
- **GATE-FIX:** add `crates/codegen/src/runtime_generator.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates` to `GENERIC_SCAN_ROOTS` (`lock14_baseline.rs:2409`) — or, post-collapse, to whatever the unified emitter file is — and extend `FORBIDDEN_GENERIC_TOKENS` with `CSS_`/`_RS` template-const patterns. Then `accepts_current_allowlist` becomes a *meaningful* green.
- **SK-V18 ASM-WAVE:** give the five scalar-passthrough kernels (F6) real NEON bodies (PMULL prefix-XOR first); wire the orphan UDOT (F7) into number parsing.
- **DISCIPLINE:** move codegen `src/lib.rs` inline tests (F8) to `tests/` per `no-inline-tests`.

---

## 9. Forward-lens note (for SK-V18 S-P0)

S-P0 must add a CH-addendum: **"gate-scope-honesty check."** A neutrality gate that PASSES is only meaningful if its scan-roots cover the files that could leak. Future S-P0 should diff `GENERIC_SCAN_ROOTS` against the actual generic-crate file inventory and flag any production `.rs` under `crates/codegen/src` (or any crate declared generic) that is NOT in the scan roots — exclusion is the leak vector, not the token list. Second addendum: **"`_neon`/`_simd` suffix truth"** — any kernel suffixed `_neon` must contain ≥1 NEON intrinsic or inline asm, else the suffix is a measurement-adjacent overstatement (relevant to AUDIT-5's bench-plane integrity since these feed the SIMD benchmark labels).
