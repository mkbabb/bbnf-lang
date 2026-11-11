# Lock-14 Firewall Audit

## Files Under Examination

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` — 5,283 lines, 66 commits
- `crates/ir/tests/lock14_grammar_generalization.rs` — 149 lines

Lock-14 states: a grammar is added by a `.bbnf` file and workspace metadata;
ZERO grammar-specific code appears in any generic crate.

---

## The Skinny Firewall Is Accreting, Not Converging

### Growth trajectory

The file was created at `sk-v8-wave0` with a single telemetry manifest gate. It
has accumulated 66 commits and is now 5,283 lines. Every campaign wave since
sk-v12 has added at minimum one new `SK_VN_WM_OWNER_PATHS` constant and a
matching arm in `validate_authorized_parent_diff`. No wave has removed or
consolidated prior constants. The function `current_lock14_owner_paths` at
lines 1318–1415 contains 47 `extend_from_slice` calls spanning sk-v8 through
sk-v18; it is append-only by construction.

Commit counts by campaign version: sk-v8 (5), sk-v13 (21), sk-v14 (26),
sk-v15 (2), sk-v18 (7). No version has a negative contribution. Each admission
is permanent.

### Contrivance ledger, not a gate

A Lock-14 gate should contain one invariant: no grammar-specific identifier
appears in a generic crate's source. This file implements something
fundamentally different: it allows a commit to edit any path in a frozen root
provided the commit subject matches a wave-specific prefix string. That design
has produced 20 commit-subject matcher functions (`is_w5b_frontend_subject`,
`is_w6_8_root_json_subject`, `is_skv18_g1_g2_emitter_subject`, and so on) and
the `validate_authorized_parent_diff` dispatch at lines 1625–2059. The function
is a 434-line cascade of `if subject.contains("sk-v14-wavew6")` guards, each
unlocking a separate path allowlist. This is a commit-message-keyed bypass
registry, not a structural invariant.

The gate inspects the single HEAD commit's subject at every run. Matches against
`sk-v8-wave2` or `sk-v10-waveW5` can never fire again — those commits are in
the past — yet their matchers and constants remain forever in the file.

### Enumerated grandfathered items

**Unreachable wave matchers (fired once, permanent residents):**

- `W2_TYPED_OWNER_PATHS` (line 617), matched by `sk-v8-wave2`.
- `W5_LOCK14_OWNER_PATHS` (line 623), matched by `sk-v8-wave5`.
- `SK_V10_W5_ROOT_TYPED_OWNER_PATHS` (line 628) and `SK_V10_W6_ROOT_TYPED_OWNER_PATHS`
  (line 637), matched by `sk-v10-waveW5` / `sk-v10-waveW6`.
- Twenty-two `SK_V13_W*` slices (lines 690–953). SK-V13 is closed.
- Fifteen `SK_V14_W*` slices (lines 956–1253). SK-V14 is closed.
- `SK_V15_W10_OWNER_PATHS` (line 1255) and `SK_V15_W11_OWNER_PATHS` (line 1261).

These dead matchers make up the majority of the 700 total path strings in the
file (333 unique paths).

**The gate file in its own allowlist:**

`crates/bbnf-bench/src/lock14_baseline.rs` appears 39 times in wave-specific
owner-path arrays. The constant `SK_V15_W11_OWNER_PATHS` at line 1261 contains
only this single path, existing solely to authorize the commit that added it.
A gate that pre-authorizes its own modification for every wave it oversees is a
gate that does not close.

**`FROZEN_ROOTS` spanning the totality tree:**

Lines 556–615 include paths prefixed with `../crates/core/` — the separate
totality workspace — such as `../crates/core/src/runtime/css_l4` and
`../xtask/runtime-projections/json.toml`. The skinny firewall exercises
editorial authority over the totality tree's grammar runtime directories, a
structural coupling that bypasses the totality gate entirely.

### SK_V18_G1_G2_EMITTER_OWNER_PATHS: the widest allowlist

The constant at lines 1284–1316 lists 25 paths including the entire source
trees of three generic crates: `"crates/grammar/src/"`, `"crates/ir/src/"`,
and `"crates/passes/src/"`. The matcher `is_skv18_g1_g2_emitter_subject` at
line 2059 admits any commit subject containing `"sk-v18-g"`, `"sk-v18-prove"`,
`"sk-v18-h1"`, or `"value-api"` (lines 2065–2069). The term `"value-api"` has
no version scope — it matches any future commit that includes that substring.
Any such commit may touch every file under the full IR, grammar, and passes
crate source trees and pass the gate.

### The GENERIC_SCAN_ROOTS deliberate exclusion

`GENERIC_SCAN_ROOTS` at lines 2353–2362 scans eight paths including
`crates/codegen/src/lib.rs`, `crates/codegen/src/lower`,
`crates/codegen/src/grammar_profile.rs`, `crates/ir/src`, and `crates/passes/src`.
The bulk of `crates/codegen/src` — `json_sink_direct/`, `json_templates/`,
`json_typed_direct.rs`, `css_scan_direct/`, and `runtime_generator.rs` — is
deliberately excluded from the full `FORBIDDEN_GENERIC_TOKENS` scan. Those
surfaces are instead routed to the narrower `CODEGEN_COURIER_SCAN_ROOTS` path
(lines 2378–2385), which checks only for `*_GENERATED_RS` suffix and
`EventGrammar` substring while explicitly permitting `JsonSink`, `JsonNodeKind`,
`JsonValue`, `JsonRoot`, and related grammar-coupled navigation tokens under the
`json_owned_roots_may_contain_json_policy_tokens` carve-out (comment at lines
2367–2377). The constants `JSON_MOD_RS`, `JSON_HOST_RS`, `CSS_MOD_RS`, and
`JSON_PARSE_ONLY_PARSER_RS` in `runtime_generator.rs` survive the gate because
they are not courier-suffix consts and do not contain `EventGrammar`.

---

## The Totality Gate Is Correct in Design

`crates/ir/tests/lock14_grammar_generalization.rs` (149 lines) implements the
invariant directly: it recursively scans every `.rs` file under
`crates/ir/src` and `crates/analysis/src`, skips doc-comment lines, and fails
if any token from the 17-member grammar-name roster appears on a code line. The
`gate_goes_red_on_reinjected_token` falsifier at line 131 proves the scanner is
load-bearing. The gate consults no commit history, maintains no wave-keyed
allowlists, and does not self-reference. The only maintenance surface is
`ROSTER`: adding a grammar adds one identifier.

Gaps in the totality gate:

- Coverage is limited to `crates/ir/src` and `crates/analysis/src`. The
  generic `passes` and `grammar` source trees are not scanned.
- The roster uses capitalized parser-struct idents (`JsonParser`, `CssL4Parser`)
  and would not catch a grammar leak expressed as a lower-case string literal
  (`"json"`) or a type alias.
- No falsifier proves that the roster is complete relative to the actual set of
  registered grammars in workspace metadata.

---

## End-State Gate Design for Totality

The end-state gate is an extended version of `lock14_grammar_generalization.rs`,
not an evolution of `lock14_baseline.rs`.

**Invariant.** Scan every production `.rs` file (doc-comment lines excluded)
under every crate that the workspace metadata declares generic. Fail if any
token from the grammar roster appears on a code line. The roster is derived at
test time from `[workspace.metadata.bbnf-strategy]` — the same source
`crates/ir/build.rs` already reads for `PRODUCTION_MANIFEST_TABLE` — so no
manual roster maintenance is required.

**Scope.** The generic crate list must be declared once, not inferred from
commit messages. For totality that list is: `crates/ir/src`,
`crates/analysis/src`, `crates/passes/src`, `crates/grammar/src`, and the
lowering surface of `crates/core`. The codegen crate's grammar-named
subdirectories cannot pass this gate until those subdirectories are eliminated.

**Self-proof requirement.** Every scan surface added to the gate must carry a
falsifier test that injects a grammar-name token on a code line and asserts the
gate fires.

**Structural prerequisites before the gate can close completely.**
Three residuals in the skinny codegen crate prevent the gate from closing today:

1. `crates/codegen/src/json_templates/` is a grammar-named directory inside a
   generic crate. It must be replaced by a grammar-agnostic template mechanism
   driven by workspace metadata or co-located outside the generic crate.

2. `runtime_generator.rs` contains `JSON_MOD_RS`, `JSON_HOST_RS`, `CSS_MOD_RS`,
   and `JSON_PARSE_ONLY_PARSER_RS` — named constants whose content embeds
   grammar-specific type identifiers. These must be generated from grammar
   metadata.

3. `crates/codegen/src/json_sink_direct/` and `crates/codegen/src/css_scan_direct/`
   are grammar-named. The existing `crates/codegen/src/general_descent/` path
   demonstrates that grammar-neutral naming is achievable; the two named emitter
   directories must be subsumed or renamed.

Until those three conditions hold, every wave that touches the codegen crate
will require a new bypass entry in `lock14_baseline.rs`. The file will accrete
indefinitely.

---

## Verdict

The skinny Lock-14 firewall is a contrivance ledger. Across 66 commits it has
accumulated 47 named wave-constant slices, 20 commit-subject matchers, a
434-line bypass-dispatch cascade, and 39 self-entries. It is append-only by
construction: every wave adds rows, no wave has removed any. The gate
self-authorizes its own modification in 39 owner-path sets and maintains
commit-subject matchers for waves that closed years ago. The correct gate exists
in embryo at `crates/ir/tests/lock14_grammar_generalization.rs` (149 lines),
which is roster-driven, structurally invariant, and needs no history. The path
to the end-state gate runs through eliminating `json_templates/`,
`json_sink_direct/`, and the grammar-named scaffold constants from the generic
codegen crate — not through another row in the allowlist.
