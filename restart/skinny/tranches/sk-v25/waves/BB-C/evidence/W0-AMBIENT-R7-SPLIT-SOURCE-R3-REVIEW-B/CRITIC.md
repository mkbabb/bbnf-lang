MODEL gpt-5.6-sol; EFFORT ultra

Runtime metadata does not independently expose or verify the active model or
reasoning effort. The declaration above is therefore not independently verified,
and the born-RED model-truth gate remains RED.

# BB-C.W0 ambient R7 split-source R3 — hostile review B

Date: 2026-07-23 EDT  
Role: fresh hostile critic B; independent read-only source review  
Host: Apple ARM64 (`uname -m` = `arm64`)

## Terminal verdict

**REJECT. R3 remains RED.** The receipt contains useful narrow repairs, but its
literal-domain and CSS claims are source-disproved, its Sheets single-authority
claim is source-disproved, and its payload/Tape/product acceptance conditions
remain explicitly or structurally unproved. It earns **zero BB-C.W0, lock, wave,
tranche, regeneration, prototype, parity, performance, convergence, or movement
credit**. Regeneration is not authorized by this review.

This verdict does not depend on another critic's findings. I did not request,
read, or use the R3 critic-A artifact.

## Custody

- Required subject:
  `W0-AMBIENT-R7-SPLIT-SOURCE-R3-SUBJECT/AUTHOR.md`, SHA-256
  `8b23cef857e87f8416ef2b787d24876a6eaab9d8e53982cd5cef0b41d7237039`
  — exact required match.
- Binding predecessor:
  `W0-AMBIENT-R7-SPLIT-SOURCE-R2-REJECTION/ADJUDICATION.md`, SHA-256
  `a4c51fd431913c63fa6a30e02f7a110653f6bf5c648f007c144f93acaad53ea7`
  — exact subject-declared match.
- The subject's lines 132–234 reproduce exactly 103 newline-terminated ledger
  rows. Independent reproduction found:
  - members: `103`;
  - bytes: `1204031`;
  - ordered path-list SHA-256:
    `56a897ab80df83d10f4583db3008ddd726d389abd28a0b39e2a8f40e6adcde76`;
  - ordered ledger SHA-256:
    `7525d13a91522ee4dc8deb602965f1c02ca4168983d7cf6b09fca7a4cc4ff090`;
  - member mismatches: `0`;
  - aggregate source lines: `32716`;
  - ledger-scoped `git diff --check`: PASS, zero output;
  - ledger-scoped `push_branch_tag` occurrences: `0`.
- Seventy-one ledger members have dirty/untracked status in the pre-existing
  worktree. This does not break custody because every current member byte matches
  the sealed ledger, but the receipt is a broad worktree snapshot rather than an
  isolated committed change.
- I read `HANDOFF.md`, `PROMPTS.md`, `INVENTORY.md`, the predecessor
  adjudication, the complete subject, BB-C W0–W7, and the BB-C/BB-D portfolio
  definitions before judgment.
- No product source, generated file, lock, package, or ledger member was edited,
  regenerated, built, formatted, staged, or committed. The only created file is
  this review.

## Material findings

### B1 — P1 — Composite numeric suffixes are erased before IR typing

The direct-literal repair is real but the submitted claim is broader than the
implementation. `MapExpr` stores only `IntLit(i64)` and `UIntLit(u64)` and has no
per-literal suffix/domain carrier
(`crates/ir/src/types/map_expr.rs:17-41`). Literal parsing range-checks a suffix
and then discards it
(`crates/core/src/lower/value_expr/literals.rs:6-67`). The only recovered
`TypeDesc` comes from `direct_numeric_suffix`, which deliberately returns a
suffix only when the **whole** map expression is one direct numeric literal
(`crates/core/src/lower/expression/wrap.rs:290-337`).

Counterexample: `-> 1i64 + 2i64` lowers to a `BinOp` of untyped `IntLit`s with
`return_type: None`; checked folding later produces untyped `IntLit(3)`.
TypeScript consequently selects `Number`, not the declared I64 `BigInt` domain
(`crates/core/src/backend/ts/projection.rs:92-124`). Wasm treats that folded
signed result as an untyped i32 and admits it rather than failing closed for I64
(`crates/core/src/backend/wasm/emitter/value.rs:43-102,145-187`). Rust happens to
default the folded signed value to I64, so the three backends no longer share the
source-declared domain.

The test suite encodes the limitation rather than closing it:
`numeric_suffix_recovery_is_confined_to_one_structural_literal` tests a direct
`1u8` and an **unsuffixed** compound expression
(`crates/core/tests/lower/expression/wrap.rs:255-290`); the external literal
tests cover direct signed minima, not suffixed composite expressions
(`crates/core/tests/literal_exactness.rs:40-119`). Claim 1 is therefore
**INCORRECT overall**, although direct signed minima, direct U64 magnitude,
finite float folding, and direct-literal backend checks are correct narrow facts.

### B2 — P1 — The CSS hash-token boundary contradicts the pinned normative snapshot

`hexHashContinuation` explicitly treats a final lone backslash as continuation
(`grammar/css/l4/color.bbnf:188-202`). The R3 test repeats that assertion by
rejecting `#abc\` both at top level and as a parent-consumable suffix
(`xtask/tests/runtime_projection_semantics.rs:341-405`).

The pinned 10 June 2026 CSS Syntax snapshot says the opposite: a backslash
followed by EOF is not a valid escape and a final backslash emits a delimiter
(`conformance/css-l4/epoch-4/inputs/objects/sha256/3f/129d17407f9bd027291bcfb8015beb86fc229bf5efe505b49ec2dc30be4390:4006-4008`).
Thus `#abc\` tokenizes as the valid three-digit hash token followed by a
backslash delimiter; R3 rejects a legal hex-color boundary and its adversarial
test locks in the error. The same rule is stated in the current primary
authority:
<https://drafts.csswg.org/css-syntax/#starts-with-a-valid-escape>.

### B3 — P1 — CSS color keywords/functions are case-sensitive and the mapped converter can panic

The new `namedColor`, function names, and `currentColor` productions are
lowercase exact literals
(`grammar/css/l4/color.bbnf:36-184,204-212,269-288`). The literal execution
surface performs byte-exact comparison
(`crates/ir/src/vm/interpreter/leaves.rs:8-21`), while CSS-defined color
identifiers and named colors are ASCII case-insensitive. Consequently legal
spellings such as `CurrentColor`, `RED`, and `RGB(...)` are rejected. The
authoritative Color 4 text expressly makes named colors case-insensitive:
<https://drafts.csswg.org/css-color-4/#named-colors>.

The conversion claim is also overstated. `try_parse_hex_color` is fallible, but
the grammar maps through `parse_hex_color`, whose return type is `u32` and whose
body calls `.expect(...)`
(`crates/core/src/runtime/css_l4/host.rs:17-52`). The production conversion is
therefore not itself total-and-fallible; a grammar/emitter divergence becomes a
panic. The R3 conversion test invokes the helper directly and cannot prove the
generated grammar-to-host boundary. Claim 5 is **INCORRECT overall**, despite the
correct narrow facts that ColorMix/HueMethod source ownership is absent, exact
3/4/6/8 conversion works, and CurrentColor/Named/functional/predefined roles are
now distinct in source.

### B4 — P2 — Sheets still has two operator-spelling authorities

The Sheets serializer imports metadata only for `comparison_expr` and uses it
only when the parent kind is `ComparisonExpr`
(`xtask/src/regen_simple_runtime.rs:1276-1327`). `tag_lexeme` separately
hard-codes `+`, `-`, `*`, `/`, `^`, `&`, and all compare spellings
(`xtask/src/regen_simple_runtime.rs:1513-1543`). Those operators also originate
in the grammar/Pratt metadata
(`grammar/google-sheets/google-sheets.bbnf:92-121`).

The external gate merely checks for the comparison metadata symbol and the
absence of one old comparison tuple
(`xtask/tests/runtime_projection_semantics.rs:35-61`); it does not reject the
remaining hard-coded table or mutate metadata to prove serializer coupling.
BBNF's binary-factor serializer is unified narrowly, but the submitted
BBNF-and-Sheets “one operator metadata authority” claim is **INCORRECT**.

### B5 — P1 — Payload cardinality asserts one for classified references that codegen can skip

The shared solver returns exactly one for every classified reference except
Scalar, without consulting transparency or whether a parse function is emitted
(`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:251-305`).
The controlling shape dispatcher, outside the ledger, explicitly skips
transparent Object/Array/Flat/Pratt/Unordered/ArgList/String/Number/Scalar
rules and emits only transparent Wrap/Keyword rules
(`crates/core/src/backend/rust/emitter/shapes/mod.rs:157-201`). Thus the claimed
“every classified reference is a one-payload boundary” is not a proved invariant:
the solver can report ONE for a boundary with no emitted callee.

The receipt also explicitly leaves Keyword map replacement, Pratt map
replacement, and TokenDispatch mapping RED
(`W0-AMBIENT-R7-SPLIT-SOURCE-R3-SUBJECT/AUTHOR.md:95-99`), while the acceptance
condition requires every material claim to close. Direct StringLit deposition
and transactional Map/Skip/Next code in the owned Flat/Inline/Wrap/Alt paths are
credible narrow repairs, but claim 2 and product-wide exactly-once payload
formation remain **UNPROVEN/INCORRECT as stated**.

### B6 — P2 — Tape ignores the identity/order token it claims to pair

`begin_compound` creates a monotonically derived `CompoundHandle`, but
`end_compound` names its argument `_handle` and simply pops the current scope
(`crates/core/src/runtime/tape/mod.rs:207-251`). It verifies neither token,
layout/rule identity, nor LIFO pairing. The public trait says the handle must be
the most recent unmatched handle but admits that the trait does not enforce it
(`crates/core/src/runtime/builder.rs:85-95`). The controlling
`CompoundHandle` has public fields and a public constructor
(`crates/core/src/runtime/handle.rs:46-58,118-126`), and that file is absent from
the receipt ledger.

Record size/alignment, checked cursor/arena reads, owned source/string bytes,
full U32 rule side-lane, U16 alternative identity, U64 sentinel separation, and
rollback restoration are correct narrow source facts. Nevertheless the
submitted Tape **identity** result and its integration custody are
**UNPROVEN/INCORRECT overall** until mismatched/forged/out-of-order handles fail
closed. The subject itself acknowledges this open condition at lines 100–102.

### B7 — P1 — Product closure and the claimed verification remain absent

The subject itself reports 886 stale generated/runtime compile errors and an
RED `xtask check-runtime`
(`W0-AMBIENT-R7-SPLIT-SOURCE-R3-SUBJECT/AUTHOR.md:86-102`). Generated grammar
and runtime bytes are intentionally outside authority. Therefore the external
end-to-end core tests cannot establish the current product, and no current
generated parser proves the repaired source contracts. The reported Cargo and
rustfmt results have no immutable log bound to the subject and were not rerun
under this read-only review; they are **UNPROVEN**, not inherited facts.

The only independent executable checks here were read-only custody/census
commands. Static token-presence tests, manually assembled IR fixtures, and a test
that encodes the wrong CSS EOF rule are insufficient substitutes for
grammar-to-IR-to-Rust/TS/Wasm execution, parse/serialize/reparse, malformed-input
no-panic, and mutation gates.

## Claim-by-claim disposition

1. **Literal exactness — INCORRECT overall.** Direct literal/minimum repairs are
   correct; suffix/domain preservation is not compositional and TS/Wasm diverge.
2. **Path-sensitive payload formation — UNPROVEN/INCORRECT as stated.** Narrow
   owned-path transactions improved; classified-reference and explicitly routed
   map/dispatch obligations do not close.
3. **Tape safety and identity — CORRECT only for the listed storage/read/rollback
   properties; INCORRECT/UNPROVEN overall.** Handle/order identity is ignored and
   its public constructor is outside custody.
4. **Fail-closed generators and serializer authority — MIXED, therefore
   INCORRECT overall.** Simple registry rejection is correct in source; Sheets
   still duplicates operator authority; generated closure is absent.
5. **CSS Level 4 boundary — INCORRECT.** The final-backslash token boundary,
   ASCII case-insensitivity, and fallible production conversion are wrong or
   unproved.
6. **Integration custody — CORRECT only for the narrow TS-emitter inclusion and
   zero ledger `push_branch_tag` census; UNPROVEN for product integration.**
   Multiple controlling consumers are outside the receipt and the generated
   fleet is stale.

## Wave/tranche fit, optimality, and friction

BB-C.W0 requires G3 deletion, typed optimizer failures, ambient egraph/CSP
writeback removal, a complete optimizer call-site inventory, and inverting
mutations (`waves/BB-C/W0.md:8-28`). R3 supplies none of those artifacts and
itself requests zero W0 credit at subject lines 104–106.

Most of this 103-file, 1.20-MB, 32.7-kLoC incident bundle belongs later:
generator/serializer work maps to BB-C.W5
(`waves/BB-C/W5.md:8-28`), while Tape/value/backend/CSS consumer closure maps
to BB-D.W0/W1 (`waves/BB-D/W0.md:8-32`,
`waves/BB-D/W1.md:8-28`). It arrives before the sealed LoweringPlan and without
generated/product closure. That ordering creates exactly the friction the
portfolio tries to remove: source contracts span IR, three backends, runtime,
generators, grammar, and tests while controlling consumers remain outside
custody and generated consumers remain stale.

The bundle is neither wave-exact nor proved parsimonious. The appropriate
disposition is to keep any accepted narrow incident fixes as prerequisite
evidence only, repair the source counterexamples above, close every explicit
RED, obtain hostile re-adjudication, and only then consider the subject's
ordered two-pass regeneration proposal. It cannot advance BB-C.W0 or the total
tranche.

## Independent commands and results

```text
uname -m
# arm64

shasum -a 256 AUTHOR.md
# 8b23cef857e87f8416ef2b787d24876a6eaab9d8e53982cd5cef0b41d7237039

shasum -a 256 ADJUDICATION.md
# a4c51fd431913c63fa6a30e02f7a110653f6bf5c648f007c144f93acaad53ea7

sed -n '132,234p' AUTHOR.md | shasum -a 256
# 7525d13a91522ee4dc8deb602965f1c02ca4168983d7cf6b09fca7a4cc4ff090

sed -n '132,234p' AUTHOR.md | awk '{print $2}' | shasum -a 256
# 56a897ab80df83d10f4583db3008ddd726d389abd28a0b39e2a8f40e6adcde76

# Per-member shasum/stat loop over all ledger rows
# members=103 bytes=1204031 mismatches=0

git diff --check -- <103 exact ledger paths>
# PASS; zero output

rg -n 'push_branch_tag' -- <103 exact ledger paths>
# zero occurrences

rg -n 'ColorMix|HueMethod|colorMix' \
  grammar/css/l4/color.bbnf xtask/runtime-projections/css_l4.toml \
  xtask/src/regen_css.rs
# zero occurrences
```

No Cargo, rustfmt, regeneration, x86, benchmark, or generated-file command was
run.

## Review digest

Review SHA-256 (canonical self-hash: hash this UTF-8 file after replacing the
64 hexadecimal digits below with 64 ASCII zeroes):

`d835e3355a36d3bf4217c461cc0b052b728fb7a41fb71eb1ccda79f7f3aef310`
