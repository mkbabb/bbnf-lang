MODEL gpt-5.6-sol; EFFORT ultra

# BB-C.W0 ambient R7 split-source R3 — hostile critic A

Date: 2026-07-23 EDT  
Role: fresh hostile source critic A  
Host: native Apple ARM64 (`arm64`), macOS 26.4.1 (25E253)

## Runtime metadata verification

The declaration above is the required critic declaration. The runtime exposed
no independent model-name or reasoning-effort metadata to this critic.
Accordingly, runtime metadata does **not** independently verify either value,
and neither is inferred from behavior, tool availability, or the dispatch
request. The model-truth gate remains RED.

## Terminal verdict

**REJECT. R3 remains RED. Isolated regeneration is not authorized.** The exact
source receipt contains semantic defects, omits controlling integration source,
and proves none of BB-C.W0's completion gates. It earns zero wave, tranche,
lock, prototype, parity, performance, convergence, or movement credit.

No source, generated fleet, lock, package, stage, commit, or Value file was
edited. No regeneration ran. This review is the sole new file created by critic
A.

## Custody

- Required subject SHA-256:
  `8b23cef857e87f8416ef2b787d24876a6eaab9d8e53982cd5cef0b41d7237039`.
- Binding R2 adjudication SHA-256:
  `a4c51fd431913c63fa6a30e02f7a110653f6bf5c648f007c144f93acaad53ea7`.
- Mandatory packet hashes:
  - `HANDOFF.md`:
    `9be82e72d82c09aae2e1dd251c9d516a6843a39868ce9853ef656ea44a9b9786`.
  - `PROMPTS.md`:
    `45f5ba8d01ebd01075517026d871c5c8cb6155b0064ecb6a5e77cce1cfa53df2`.
  - `INVENTORY.md`:
    `cc1271b112c5fd408e8c4ff55297d4afaa57c4157560e3eb54b3a9fd76f89b28`.
- Independent reconstruction of the subject ledger produced exactly 103
  members and 1,204,031 bytes, ordered path digest
  `56a897ab80df83d10f4583db3008ddd726d389abd28a0b39e2a8f40e6adcde76`,
  ordered `shasum -a 256` ledger digest
  `7525d13a91522ee4dc8deb602965f1c02ca4168983d7cf6b09fca7a4cc4ff090`,
  and zero member mismatches. A final recheck immediately before review
  creation reproduced all four values.
- The exact 103 members contain zero `push_branch_tag` occurrences.
- Canonical review SHA-256 (replace the 64 hex characters on this line with 64
  ASCII zeroes before hashing):
  `42f6bd0db5463fbebc6943c3e5ae79e0828a2e1c0691e5f9807f5f5218f7e834`.

## Prioritized findings

### P0 — the shared payload authority mis-emits successful paths

Claim 2 is **INCORRECT**, not merely incomplete.

1. The AltDispatch pure-chain gate admits every `Regex` at
   `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:775-805`,
   but its emitted regex arm at `:863-877` never reads the regex ID or pattern.
   It hard-codes a non-empty ASCII-alphanumeric-or-underscore scan. A grammar
   regex such as punctuation, whitespace, a quoted-string body, or a bounded
   class therefore receives different recognition semantics.
2. The same fast path flattens `OptionalWhitespace` away at
   `branches.rs:884-909`. This contradicts the dedicated structural emitter's
   documented and implemented requirement to emit leading and trailing
   `skip_space` at
   `crates/core/src/backend/rust/emitter/shapes/inline/structural_branch.rs:45-51,159-165`.
   A pure literal/regex sequence can therefore lose `?w` semantics solely
   because it took the fast path.
3. The cardinality domain is only `{zero, one, many}` and deliberately caps
   sums at `many` (`branches.rs:166-243`). Flat then admits any nonzero
   `many` result against any Struct layout with more than one field
   (`crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs:244-260`).
   Three committed payloads are thus accepted against a two-field solved
   layout. This is not exact aggregate cardinality. The cited regression at
   `crates/core/tests/emitter_typed_payloads.rs:1051-1094` proves only the
   matching two-payload/two-field example.
4. TokenDispatch is not a conservative semantic implementation: Inline returns
   `Syntax` unconditionally
   (`inline/structural_branch.rs:380-386`), while Flat delegates through the
   dispatcher fallback (`flat/struct_direct.rs:742-749`). The subject itself
   routes TokenDispatch mapping as unproved. Under the governing rule that any
   defect or unproved path keeps RED, no path-total cardinality authority exists.

The constant/StringLit replacement helper at
`branches.rs:137-163`, its checkpoint/rollback use in Flat and Inline, and the
Skip/Next discarded-side checkpoints are correct narrow repairs. They do not
cure the regex, whitespace, aggregate, or TokenDispatch counterexamples.

### P0 — the stated CSS Level 4 boundary is not normative and hex integration is not fallible

Claim 5 is **INCORRECT overall**.

- The grammar calls
  `crate::css_types::parse_hex_color(input) : u32` at
  `grammar/css/l4/color.bbnf:198-202`. The production host entry point returns
  `u32` and calls `Option::expect` at
  `crates/core/src/runtime/css_l4/host.rs:17-19`; only a separate helper at
  `:21-52` is fallible. HRegex emits a direct `u32` call at
  `crates/core/src/backend/rust/emitter/shapes/hregex.rs:361-380`.
  Consequently the integrated conversion is panic-based, directly
  contradicting “total and fallible.” The passing test at
  `xtask/tests/runtime_projection_semantics.rs:221-256` exercises only the
  helper.
- The frozen CSS Color 4 source defines
  `<color> = <color-base> | currentColor | <system-color>` at the frozen
  snapshot
  `conformance/css-l4/epoch-4/inputs/objects/sha256/f5/b6af...:1003-1012`.
  The receipt's top-level production has only currentColor, color(), hex,
  functional, and named branches (`color.bbnf:269-288`); every system-color is
  absent.
- Named colors are CSS idents and ASCII case-insensitive in the same frozen
  source at `:1560-1568`, but `color.bbnf:36-184` lists only lowercase byte
  literals. The receipt's literal emitters compare exact bytes
  (`alt_dispatch/branches.rs:577-594`) and contain no case-folding authority.
  Valid `Red` is rejected. The same problem applies to CSS function keywords.
- One generic production forces every functional family through three
  `number|percentage` components (`color.bbnf:31-32,209-240`). CSS Color 4
  admits hue angles and `none`, including the frozen HSL grammar at
  `:1113-1128,2235-2253`; examples such as `hsl(30deg 100% 50%)` are outside
  the receipt grammar.

Deleting ColorMix/HueMethod, separating CurrentColor/function/predefined
owners, the four exact hex widths and hash continuation, percentage deposition,
and the 49 configured length-unit tags are correct narrow source facts. Two
lowercase happy-path role checks at
`crates/core/tests/css_l4_color_roles.rs:26-35` and source-substring assertions
at `runtime_projection_semantics.rs:70-179` do not prove a Level 4 boundary.

### P1 — integer suffix/type identity is preserved only for a direct root literal

Claim 1 is **INCORRECT overall**.

`parse_int_literal` validates a suffix and returns only
`MapExpr::IntLit(i64)` or `UIntLit(u64)`
(`crates/core/src/lower/value_expr/literals.rs:6-68`). `MapExpr` has no
per-literal width/suffix carrier
(`crates/ir/src/types/map_expr.rs:17-41`). Lowering reconstructs a return type
only when the **entire** lowered expression is a literal and the complete RHS
source ends in that suffix
(`crates/core/src/lower/expression/wrap.rs:290-337`).

Therefore `-> (1i8)` lowers to an `IntLit` whose RHS ends in `)`, and a
composite such as `-> 1i8 + 2i8` lowers to a `BinOp`; both lose the literal's
`i8` identity. The tests at
`crates/core/tests/literal_exactness.rs:40-137` cover only direct-root
spellings. Direct signed minima, exact UInt transport, checked constant folds,
TypeScript bigint projection, and Wasm rejection of unsupported widths are
correct narrow facts, but they do not establish the submitted
grammar-to-IR suffix claim.

StringLit's direct Rust semantic deposit is correct narrowly. TypeScript
StringLit exactness is additionally unproved by the receipt: `ts_escape`
emits NUL as `\\0` (`backend/ts/projection.rs:47-59`), while the controlling
unescape implementation (outside the ledger) permits `\\0`
(`backend/util.rs:7-27`). A semantic NUL followed by an ASCII digit is emitted
as a legacy-octal/strict-invalid JavaScript escape such as `"\\01"`; the only
receipt test covers newline (`literal_exactness.rs:188-203`).

### P1 — the exact ledger is not a complete integration boundary

Claim 6 is **INCORRECT as custody**, although its two literal scoped facts are
correct: the TypeScript call-site file is present, and the 103 governed members
contain no `push_branch_tag`.

The binding R2 order required every controlling integration file. R3 still
omits:

- `crates/core/src/backend/rust/emitter/shapes/scalar.rs`, even though claim 2
  depends explicitly on exact Scalar literal/null/bool/alias behavior;
- `crates/core/src/backend/util.rs`, which `backend/ts/projection.rs:63-65`
  directly calls for StringLit unescaping;
- `crates/core/src/backend/rust/emitter/regex_scan_adapter.rs`, which the
  governed HRegex, Flat, and Inline files call for regex recognition; and
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs`, which
  supplies their `skip_space` behavior and was dirty but unsealed at review
  time (review-time SHA-256
  `86fbbed55f9a80f6c77337681e2e0b8816355078e27df7fb4eee0e380c7070d8`).

Those omissions prevent the exact 103-member hash from isolating the source
that a regeneration would consume. Generated grammar/runtime files are
separately and correctly disclosed as stale; that disclosure keeps product
closure RED rather than proving integration.

### P1 — Tape's enumerated repairs are real, but its safety title remains overbroad

Claim 3's enumerated facts are **CORRECT narrowly**: `TapeRec` is 16-byte,
align-4 (`runtime/tape/record.rs:90-118`); cursor/reference constructors are
crate-sealed and checked (`runtime/tape/cursor.rs:16-65`); arena reads are
range-checked (`runtime/tape/arena.rs:95-127`); input/string bytes are owned;
rule/branch/u64 domains and checkpoint restoration are preserved.

Complete Tape formation is nevertheless **UNPROVEN/INCORRECT as a safety
umbrella**. `end_compound` ignores its `CompoundHandle` and blindly pops the
innermost scope (`runtime/tape/mod.rs:223-251`), so swapped, stale, or duplicate
handles can silently close the wrong compound. Bounds-end likewise ignores a
missing start (`:195-203`). No adversarial test covers either misuse. The
subject acknowledges the handle/order obligation, so it cannot simultaneously
support regeneration under an “any unproved path keeps RED” gate.

The additional four-byte rule-id lane, full input copy, cloned scope/bounds
stacks per checkpoint, and allocated child-offset vector
(`cursor.rs:232-270`) also preclude performance or parsimony credit; the subject
properly disclaims such credit.

## Claim-by-claim disposition

1. **Literal exactness — INCORRECT overall.** Direct signed-minimum, UInt,
   checked-fold, TS-bigint, and Wasm-fail-closed cases are correct; suffix/type
   preservation is not compositional, and TS StringLit escaping is incomplete.
2. **Path-sensitive payload formation — INCORRECT.** Constant/StringLit
   replacement and Skip/Next rollback are correct narrow cases; Regex,
   OptionalWhitespace, aggregate cardinality, and TokenDispatch defeat totality.
3. **Tape safety and identity — CORRECT only as enumerated; complete formation
   UNPROVEN/INCORRECT.** Handle/order integrity remains defective.
4. **Fail-closed generators and serializer authority — CORRECT in narrow source
   formation; behavioral parity UNPROVEN.** Simple recursively rejects
   unsupported scalar types at `xtask/src/regen_simple_runtime.rs:333-395`,
   and both serializers consult `PRECEDENCE_ENTRIES_*` at
   `:1280-1325,1513-1522,3732-3809,4031-4040`. The seven passing xtask tests
   mostly render source or assert substrings; the product does not compile.
5. **CSS Level 4 boundary — INCORRECT overall.** Several requested role/hex/unit
   source repairs are correct, but integrated conversion is infallible and
   normative Color 4 syntax is materially missing.
6. **Integration custody — INCORRECT.** The TS call site and scoped no-alias
   census are correct; controlling Scalar, unescape, regex, and whitespace
   sources remain outside exact custody.

## Verification commands and results

All executable checks were native Apple ARM. No x86 command, source edit,
regeneration, staging, commit, or Value contact occurred.

- Independent Python read/hash reconstruction of `AUTHOR.md` ledger:
  `103`, `1204031`, path digest `56a897...de76`, ledger digest
  `7525d1...e090`, zero mismatches, zero scoped `push_branch_tag`.
- `cargo test -p bbnf-ir --test map_expr_uint --test struct_registry`:
  **PASS**, 4/4 and 14/14.
- `cargo test -p xtask --test runtime_projection_semantics`:
  **PASS**, 7/7.
- `cargo check -p xtask`: **PASS**, with only the reported unused
  `LIVE_TOOL_NAMES` warning.
- `git diff --check`: **PASS**.
- `cargo test -p bbnf --test literal_exactness --no-run`:
  **expected RED**, exactly 886 errors and one warning. The failures include
  stale `push_branch_tag`, old branch/layout types, missing U8/UInt runtime
  surfaces, and stale Pratt/runtime views.

The passing isolated tests validate narrow helpers. The 886-error product gate
means no Rust parser, external serializer round trip, CSS no-throw behavior, or
generated integration was exercised.

## BB-C.W0 / tranche / parsimony ruling

BB-C.W0 requires G3 deletion with its counterexample, typed optimizer failure
reports, ambient egraph/CSP writeback removal, a complete optimizer call-site
inventory, and inverting mutations
(`waves/BB-C/W0.md:8-28`; `TRANCHE-PORTFOLIO.md:228-239`). R3 supplies none of
those. Its 103-file, 1.20-MB literal/payload/Tape/CSS/generator incident slice is
not proved minimal, wave-optimal, or a bounded optimizer-removal implementation.
The subject correctly calls itself prerequisite evidence and claims zero W0
credit; this hostile review preserves that ruling.

Repair the source-proved blockers and seal every controlling source input before
another receipt. Only a later accepted receipt may authorize the ordered
grammar-to-sidecar-to-runtime regeneration, byte-identical second pass, full
native compile, behavioral parity/no-throw/corpus gates, and product-wide
legacy census.
