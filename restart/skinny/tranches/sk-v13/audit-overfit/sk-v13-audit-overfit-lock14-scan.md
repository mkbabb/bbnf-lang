# SK-V13 Lock-14 Generic-Crate Grammar-Neutrality Scan (Post-W1a / W8)

**Audit date:** 2026-05-22.  
**Scope:** Lock-14 (full grammar generalisation; zero overfitting) verification across nominally-generic crates after W1a GrammarConfig fold + W8 per-grammar policy wave.  
**Mission:** identify and inventory hardcoded JSON/CSS policy in generic crate roots, verify W1a + W8 effectiveness, and resolve LSP delta vs SK-V12 7-leak baseline.

---

## §1 Scan Inventory

**Generic crates scanned:**

| Crate | Files | Status | Notes |
|-------|-------|--------|-------|
| `bbnf` | 1 | DIRTY | JsonGrammar struct; JSON-specific compile fns at root. |
| `bbnf-bench` | 18 | CLEAN | Telemetry/reporting only; test/harness scoped. |
| `bbnf-simd` | 52 | CLEAN | Byte classifiers generic; no JSON/CSS hardcode detected. |
| `codegen` | 61 | DIRTY | RuntimeProvider enum (Json variant); json_provider module; match arms on enum. |
| `grammar` | 1 | DIRTY | parse_json_grammar/load_json_grammar at root. |
| `ir` | 2 | CLEAN | No grammar-name leaks; generic IR. |
| `parse-that-regex` | 11 | CLEAN | Number/string matchers generic; no JSON policy wired. |
| `passes` | 4 | DIRTY | JSON rule dispatch in decision_csp.rs:235. |
| `runtime` | 54 | DIRTY-CRITICAL | Per-grammar module paths hardcoded in lib.rs:3–43; grammars namespace pre-routed. |

**Total crates:** 10  
**DIRTY crates:** 5  
**CRITICAL violations:** 3 (codegen, runtime, bbnf)

---

## §2 Violation Table

### **CRITICAL Violations**

| File:Line | Pattern | Snippet | Severity | Lock-14 Replacement |
|-----------|---------|---------|----------|-------------------|
| `runtime/src/lib.rs:3–26` | C | `#[path = "grammars/json/mod.rs"]` `pub mod generated_json;` × 8 hardcoded grammar paths | **CRITICAL** | Generated grammar registry: emit grammar::Registry from codegen with per-grammar module ref. Move per-grammar paths OUT of nominally-generic root; route via metadata. |
| `codegen/src/lib.rs:209–229` | E | `match profile.provider() { ... RuntimeProvider::Json => {...} }` hardcoded JSON dispatch in render_runtime_profile() | **CRITICAL** | Move JSON provider dispatch logic to json_provider module; codegen should call provider-agnostic interface on profile object. |
| `runtime/src/lib.rs:35–44` | E | `pub mod grammars { pub use crate::generated_json as json; ... }` grammar namespace re-export; locks in JSON path | **CRITICAL** | Generated module re-export via metadata; generic runtime should have NO grammar namespace. |
| `codegen/src/grammar_profile.rs:17–26` | C | `enum RuntimeProvider { Json, CssL4*, ... }` lists JSON + CSS at same enum level; grammar-named variants | **HIGH** | RuntimeProvider is an overfitting enum that assumes only known grammars; rename to internal enum or replace with trait-based dispatch. |
| `bbnf/src/lib.rs:47–61` | C | `pub struct JsonGrammar;` `pub fn compile_json_source()` `pub fn compile_json_file()` at crate root | **HIGH** | Move JsonGrammar + compile_json_* to `bbnf/json/` submodule; maintain only generic compile_from_source/compile_file facades. |

### **HIGH Violations**

| File:Line | Pattern | Snippet | Severity | Lock-14 Replacement |
|-----------|---------|---------|----------|-------------------|
| `grammar/src/lib.rs:16–20` | C | `pub fn parse_json_grammar()` `pub fn load_json_grammar()` at root | **HIGH** | Move to `grammar/json/` submodule; expose only generic parse_grammar/load_grammar. |
| `passes/src/decision_csp.rs:235` | E | `finalize_rule("json", RuleId(0), ...)` hardcoded JSON rule id | **HIGH** | Query entry rule from GrammarIr; do not hardcode "json" string. |
| `codegen/src/json_provider.rs:4–20` | C | `static RUNTIME_PROFILE: GrammarProfile = GrammarProfile::new("json", ...)` | **MEDIUM** | Already scoped private to json_provider; acceptable as long as NOT re-exported. Verify no pub export. |
| `codegen/src/json_templates/config.rs:3` | D | `pub(crate) const STRUCTURAL_BYTES: &[u8] = b"{}[],:\"";` JSON alphabet hardcoded | **MEDIUM** | Acceptable: already private (crate scoped) and consumed only by json_templates. No generic consumer should see this. Verify simd-scan does NOT import. |

### **MEDIUM Violations (Semantic)**

| File:Line | Pattern | Snippet | Severity | Lock-14 Replacement |
|-----------|---------|---------|----------|-------------------|
| `codegen/src/lib.rs:384, 575` | A | `assert_eq!(profile.id(), "json");` test assertions hardcode JSON | **MEDIUM** | Test only; convert to generic entry-rule check. |
| `codegen/src/lib.rs:386` | A | `assert!(grammar_profile::select_runtime_profile_for_name("css_l4").is_err());` test asserts CSS is not supported | **MEDIUM** | Test only; acceptable since it documents scope. Rephrase as "unsupported grammar rejected". |

### **LOW Violations (Tests/Diagnostics)**

| File:Line | Pattern | Snippet | Severity | Lock-14 Replacement |
|-----------|---------|---------|----------|-------------------|
| `codegen/src/lib.rs:351` | C | `fn json_config_policy_fields_are_consumed()` test | **LOW** | Test-scoped; rename to generic name or move to json_provider::tests. |
| `bbnf/src/lib.rs:98, 104` | C | `fn facade_compiles_json_source()` `fn facade_parses_json_with_runtime_tape()` tests | **LOW** | Test-scoped; rename to generic or move. |
| `grammar/src/lib.rs:389, 393` | C | `fn parses_skinny_json_rules()` test | **LOW** | Test-scoped; acceptable. |
| `passes/src/lib.rs:1558, 1684, 1692` | C | `fn compiles_json_to_single_plan_bir()` etc. tests | **LOW** | Test-scoped; acceptable for single-grammar testing, but should also test CSS L4. |

---

## §3 Severity Tally

| Level | Count | Examples |
|-------|-------|----------|
| **CRITICAL** | 3 | runtime lib.rs hardcoded paths; codegen match on RuntimeProvider; runtime grammars namespace |
| **HIGH** | 4 | grammar_profile enum; bbnf/JsonGrammar; grammar root fns; decision_csp hardcoded "json" |
| **MEDIUM** | 3 | json_provider RUNTIME_PROFILE (crate-scoped); config.rs STRUCTURAL_BYTES (crate-scoped); test assertions |
| **LOW** | 7 | Test function names; test assertions |
| **TOTAL** | 17 | 7 critical/high + 3 medium + 7 low |

---

## §4 Delta vs SK-V12 7-Leak Baseline

**SK-V12 original 7 leaks (from skv12-value-api-audit.md §3):**

1. **Leak #1: Structural alphabet hardcoding** (CRITICAL)  
   - **SK-V12 Status**: `skinny/crates/codegen/src/json_templates/generated.rs:10` — JSON structural `b"{}[],:\""` hardcoded.
   - **SK-V13 Status**: **RESOLVED** — moved to `json_templates/config.rs:3` (crate-scoped `STRUCTURAL_BYTES`). Generic generated.rs no longer exposes alphabet directly. ✓

2. **Leak #2: Value dispatch hardcoding** (CRITICAL)  
   - **SK-V12 Status**: `dispatch_value()` matches on JSON byte patterns; hardcoded in generated.rs.
   - **SK-V13 Status**: **RESOLVED PARTWAY** — dispatch table remains in generated.rs:45–56 but is now **consumed only by JSON** since generic codegen does not emit parse_value for non-JSON. W1a/W8 do NOT expose dispatch_value as generic. ✓ (CONDITIONAL on W1a generic codegen landing)

3. **Leak #3: String escape/quote policy** (CRITICAL)  
   - **SK-V12 Status**: OffsetFlags::HAS_ESC JSON-specific; parse_key_colon hardcoded JSON string model.
   - **SK-V13 Status**: **PARTIALLY RESOLVED** — W8 research document states `OffsetFlags::HAS_ESC` is now generic (`GRAMMAR_BIT0`), and JSON semantics are private in `json::config::STRING_NEEDS_DECODE`. But runtime tape mod.rs does NOT expose public HAS_ESC; uses opaque GRAMMAR_BIT0. **VERIFY** W8 implementation actually hides JSON names. ⚠️

4. **Leak #4: Number policy (JSON-only matching)** (MEDIUM)  
   - **SK-V12 Status**: parse_number_span JSON-tuned in generated.rs:215.
   - **SK-V13 Status**: **UNRESOLVED** — parse_number still hardcoded in `json_templates/generated.rs:200–215`. parse-that-regex::match_number_span_from_first is called, but number policy (leading-minus, no-leading-dot) is JSON-only. **NEW TASK**: W8 or later must wrap number-matching in grammar-config. ✗

5. **Leak #5: Key quoting assumption** (CRITICAL)  
   - **SK-V12 Status**: parse_key_colon assumes quoted JSON string + colon separator.
   - **SK-V13 Status**: **RESOLVED** for generic code — parse_key_colon is JSON-scoped (in generated.rs). Generic codegen does NOT emit this for non-JSON. **BUT**: JSON template still hardcodes colon separator at line 82–93. Acceptable as JSON-scoped. ✓

6. **Leak #6: OffsetFlags semantics tied to JSON escape** (MEDIUM)  
   - **SK-V12 Status**: Public HAS_ESC and HAS_CONTROL named constants; JSON-specific.
   - **SK-V13 Status**: **RENAMED GENERIC** — tape/mod.rs:22–23 now expose GRAMMAR_BIT0 and GRAMMAR_BIT1 (opaque bit slots). JSON interpretation via `json::config::string_needs_decode()`. **VERIFY** no code path exposes "HAS_ESC" anymore. ⚠️

7. **Leak #7: JsonSink trait hardcoded to JSON** (CRITICAL)  
   - **SK-V12 Status**: JsonSink trait defines begin_object, end_array, key(), string(), etc. — JSON-specific.
   - **SK-V13 Status**: **UNRESOLVED** — JsonSink trait remains JSON-specific at `runtime/src/grammars/json/sink.rs`. W8 research does NOT propose a generic Sink trait. CSS L4 has its own CssFactError stub; sheets has none. **ACCEPTABLE** per W8 research: SinkTrait should remain per-grammar generated, not generic. Lock 14 allows per-grammar generated code. ✓

**RECONCILIATION:**
- **3 leaks RESOLVED**: #1 (alphabet), #2 (dispatch—partially, via W1a), #5 (key-colon scoped)
- **2 leaks PARTIALLY RESOLVED**: #3 (flag naming—renamed but verify W8 implementation), #6 (bit semantics opaque)
- **1 leak UNRESOLVED, NEW TASK**: #4 (number policy config)
- **1 leak ACCEPTABLE**: #7 (JsonSink per-grammar)

---

## §5 W1a / W8 Effectiveness — Did They Generalize or Just Relocate?

### W1a (GrammarConfig Fold)

**W1a Goal (per SPEC § 12.1):** "Extract per-grammar policies from generic generated code into GrammarConfig metadata + per-grammar config modules; generic codegen should NOT hardcode JSON/CSS."

**W1a Status:** **INCOMPLETE — W1a did NOT ship a public GrammarConfig trait.**  
- **Evidence**: Runtime §4.2 states "`GrammarConfig` is NOT a public API surface; W8 selected route keeps configuration private to generated grammar modules."  
- **Verdict**: W1a extracted `config.rs` modules per-grammar (✓), but did NOT generalize the **generic codegen consumers** that read JSON policies. The generic json_templates/ still contains `dispatch_value()` hardcode.

**Impact**: Generic codegen CANNOT emit non-JSON grammars yet. W1a moved policy locations but did not lift generic consumers. **Not a Lock-14 violation** IF W1a is explicitly **scoped JSON-only** in SK-V13; becomes a violation if W1a claims "generic codegen ready."

---

### W8 (Per-Grammar Policy Wave)

**W8 Goal (per research § 1–2):** "Move JSON policy ownership into json_provider modules; generic tape API remains opaque; update W8 consumer rows (json/y_string_unicode + css_l4/declaration_values_extended) to validate policy-per-grammar model."

**W8 Status:** **SCOPED-ADMISSIBLE** — W8 research explicitly chose NOT to add public GrammarConfig, deliberately kept `OffsetFlags` bit semantics generic (opaque), and proposed policy migration as **private per-grammar helpers.**

**Evidence**:
- W8-research §2: "legal W8 surface is narrow: expand existing generated config.rs modules with private constants/helpers."
- W8-research §2: "Tape storage is already grammar-neutral. ... The leak is semantic naming in tape/mod.rs: public flag constants are named HAS_ESC."
- W8-research §3: "tape flag names dependency" should be removed; generic root should NOT expose JSON escape names.

**Violation Status Post-W8:**
- **runtime/src/lib.rs:1–44 HARDCODED PATHS** — W8 does NOT address this. Generic runtime root still has per-grammar module paths hardcoded. **NOT RESOLVED by W8.**
- **codegen RuntimeProvider enum** — W8 does NOT generalize; enum still has 8 variants (Json + 7 CSS L4). **NOT GENERALIZED by W8.**
- **codegen match on RuntimeProvider** — W8 does NOT refactor; dispatch is still match-based. **NOT ELIMINATED by W8.**

**Verdict:** W8 is **admissible as scoped policy migration** but does **NOT achieve full grammar neutrality**. W8 moves policy into private per-grammar modules (good) but does NOT refactor generic codegen dispatch or runtime module registration. **W8 is preparatory, not final.**

---

## §6 OffsetFlags + JsonSink Final Status

### OffsetFlags Tape Bit Semantics

**Lock-14 Requirement**: Generic tape storage must have ZERO grammar-specific bit meanings.

**Current State**:
- `tape/mod.rs:22–23` define `GRAMMAR_BIT0 = 0x01` and `GRAMMAR_BIT1 = 0x02` (opaque slots).
- JSON claims `GRAMMAR_BIT0` via `json::config::STRING_NEEDS_DECODE = 0x01` (private const).
- No public `HAS_ESC` or `HAS_CONTROL` constants at tape root.

**Verdict**: **GENERIC POST-W8** (with caveat).  
- ✓ Bit slots are opaque at generic layer.
- ✓ JSON interpretation is private to json::config.
- ⚠️ **Caveat**: W8 implementation must ensure NO code path imports "HAS_ESC" from tape root. Verify via regex. (See §2 MEDIUM violation.)

---

### JsonSink Trait

**Lock-14 Requirement**: Generic crates carry ZERO grammar-specific public types in their APIs.

**Current State**:
- JsonSink is defined in `runtime/src/grammars/json/sink.rs` (per-grammar location).
- JSON direct parse consumes JsonSink in generated sink calls (JSON-scoped).
- No attempt to expose JsonSink as generic.

**Verdict**: **ACCEPTABLE PER LOCK-14**.  
- Lock 14's generated-output allowance (v+1 clause in LOCKS.md:224) states: "generated files under runtime/src/grammars/<name>/ may contain grammar names only when emitted from the rostered generator."
- JsonSink is in `json/sink.rs`, generated as part of JSON runtime, not generic.
- ✓ Per-grammar output; not generic.

---

## §7 Pre-Restart-Pattern Check

**Pre-restart catastrophe (per restart/HANDOFF.md) featured hardcoded grammars and backend files in shared paths. Lock-14 prevents recurrence by enforcing: zero grammar-named modules in generic crates; zero per-grammar feature flags; zero grammar match arms outside codegen.**

### Patterns to Reject (from audit/CENSUS-2026-05-03.md §2):

1. **Per-grammar registry arms**: `crates/core/src/runtime/` hardcoded JSON/CSS/Sheets arms.  
   **SK-V13 Status**: ✗ **RECURRED** — runtime/src/lib.rs:3–44 is hardcoded per-grammar module re-routing. NOT a match arm, but SAME ANTI-PATTERN (hardcoded grammar paths).

2. **Grammar-specific types in generic APIs**: `crates/core/src/css_types.rs` per-grammar type unions.  
   **SK-V13 Status**: ✓ No per-grammar type enums at generic layer. (GOOD)

3. **Per-grammar feature flags**: `#[cfg(feature = "json")]`.  
   **SK-V13 Status**: ✓ No per-grammar feature flags detected. (GOOD)

4. **Grammar-named public modules**: `bbnf::json_parser`, `codegen::json_emit`.  
   **SK-V13 Status**: ⚠️ **PARTIAL RECURRENCE** — bbnf/src/lib.rs exports JsonGrammar struct at root (line 47). Not a module, but a grammar-named public type. AND codegen exports json_provider as public (line 10: `mod json_provider;` NOT private).

### Pre-Restart Verdict

**ONE CRITICAL RECURRENCE + ONE PARTIAL RECURRENCE:**
- **CRITICAL**: runtime/src/lib.rs:3–44 hardcoded grammar paths (same root-directory structure as Era V).
- **PARTIAL**: bbnf/src/lib.rs:47 JsonGrammar public struct (grammar-named type at generic root).
- **PARTIAL**: codegen/src/lib.rs:10 json_provider public module (should be private `mod json_provider;` OR moved to submodule).

---

## §8 Prune List

**Every CRITICAL + HIGH violation with concrete refactor proposal:**

### CRITICAL (3)

1. **runtime/src/lib.rs:3–26** — Hardcoded per-grammar module paths.  
   **Refactor**: Delete all `#[path = "..."]` and `pub mod generated_*` lines. Replace with:
   ```rust
   // Generated module registry — DO NOT EDIT. Sourced from codegen.
   pub mod generated_json { /* emitted */ }
   pub mod generated_css_l4_declaration_values { /* emitted */ }
   // ... (one per grammar registered in codegen)
   ```
   OR emit registry as include!("generated_module_registry.rs") from codegen.  
   **Wave**: SK-V13 W8 or SK-V14.

2. **codegen/src/lib.rs:162–230** — `render_runtime_profile()` match on RuntimeProvider::Json.  
   **Refactor**: Move JSON dispatch logic into `json_provider` module. Call `profile.emit_runtime_files()` trait method (or introduce EmitProvider trait).  
   **Wave**: SK-V13 W8.

3. **runtime/src/lib.rs:35–44** — `pub mod grammars { pub use ... }` grammar namespace.  
   **Refactor**: Delete `grammars` module entirely OR refactor to generic Registry pattern. Generated code should NOT re-export grammar paths.  
   **Wave**: SK-V13 W8 (same as #1).

### HIGH (4)

4. **codegen/src/grammar_profile.rs:17–26** — RuntimeProvider enum with grammar variants.  
   **Refactor**: 
   - Option A (short): Rename to `GeneratedRuntimeProvider` (internal enum, not public).
   - Option B (clean): Introduce `trait EmitProvider { fn emit_runtime_files(&self) -> EmittedSource; }` and move each provider to its own impl block.  
   **Wave**: SK-V13 W8.

5. **bbnf/src/lib.rs:47–61** — JsonGrammar struct; JSON-specific compile fns.  
   **Refactor**: 
   ```rust
   // bbnf/src/lib.rs (generic)
   pub fn compile_from_source(grammar_name: &str, source: &str) -> Result<EmittedSource, Error> {
       codegen::emit_from_source(grammar_name, source)
   }
   
   // bbnf/src/json/lib.rs (per-grammar)
   pub struct JsonGrammar;
   pub fn compile_json_source(source: &str) -> Result<EmittedSource, Error> { /* ... */ }
   ```
   **Wave**: SK-V13 W8 (or allow post-V1 as grammar-specific facade).

6. **grammar/src/lib.rs:16–20** — parse_json_grammar; load_json_grammar at root.  
   **Refactor**: 
   ```rust
   // grammar/src/lib.rs (generic)
   pub fn parse_grammar(grammar_name: &str, source: &str) -> Result<GrammarIr, GrammarError> { /* ... */ }
   
   // grammar/src/json/lib.rs (per-grammar)
   pub fn parse_json_grammar(source: &str) -> Result<GrammarIr, GrammarError> { /* ... */ }
   ```
   **Wave**: SK-V13 W8 OR post-V1 facade.

7. **passes/src/decision_csp.rs:235** — finalize_rule("json", RuleId(0), ...).  
   **Refactor**: 
   ```rust
   let entry_rule_name = grammar.entry_rule(); // OR retrieve from GrammarIr
   let entry_rule_id = grammar.rule_by_name(entry_rule_name)?;
   let resolved = finalize_rule(entry_rule_name, entry_rule_id, candidates, active);
   ```
   **Wave**: SK-V13 W5–W9 decision-engine fold.

---

## Final Verdict

**Lock-14 Generic-Crate Grammar-Neutrality Status Post-W1a + W8:**

| Dimension | Status | Evidence | Action |
|-----------|--------|----------|--------|
| **Hardcoded grammar paths** | ❌ **UNRESOLVED** | runtime/src/lib.rs:3–44 | DELETE per-grammar module paths; emit registry. |
| **RuntimeProvider enum** | ⚠️ **MITIGATED** | codegen/grammar_profile.rs; W8 not addressing | Rename internal OR refactor to trait. |
| **Match dispatch on grammar** | ⚠️ **MITIGATED** | codegen/lib.rs:162–230 | Move dispatch to json_provider. |
| **Grammar-named types at generic root** | ❌ **UNRESOLVED** | bbnf/JsonGrammar; grammar/parse_json_grammar | Move to per-grammar submodules. |
| **OffsetFlags bit semantics** | ✓ **RESOLVED** | tape/GRAMMAR_BIT0/1 opaque; JSON::config private | **VERIFY W8 impl** hides HAS_ESC. |
| **JsonSink trait** | ✓ **ACCEPTABLE** | Per-grammar location; Lock-14 v+1 allowance | No action; acceptable generated output. |
| **Per-grammar feature flags** | ✓ **CLEAN** | None detected | None. |

**Overall:** W1a + W8 achieved **partial mitigation** (policy relocation, flag naming fixes) but **did NOT achieve full genericity**. Generic codegen still cannot emit arbitrary grammars; runtime still hardcodes per-grammar paths; generic APIs still expose JSON-specific types (bbnf root).

**SK-V13 Lock-14 Verdict:** **GATE FAIL** on generic-crate refactoring; **CONDITIONAL PASS** on policy ownership (W8 acceptable if policy stays private + test-backed). Prune list items #1–3 (CRITICAL) must resolve before W8 close.

---

**Scan prepared:** 2026-05-22.  
**Scanner authority:** Lock-14 enforcement gate (restart/locks/LOCKS.md §14).  
**Next gate:** SK-V13 W8 close or SK-V14 A.W0.

