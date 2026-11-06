MODEL gpt-5.6-sol; EFFORT ultra

# BB-C.W0 ambient R7 split-source R3 — adjudication

Date: 2026-07-23 EDT  
Role: fresh third adjudicator  
Host: native Apple ARM64 (`uname -m` = `arm64`; `arch` = `arm64`)

## Model-truth gate

Independent runtime metadata verification for this adjudicator: **YES**.
`nodeRepl.requestMeta._meta["x-codex-turn-metadata"]` independently exposed
`model = "gpt-5.6-sol"` and `reasoning_effort = "ultra"`. Those values are not
inferred from dispatch text, tools, behavior, or the declaration above.

That does not retroactively verify the implementation authors or critics A and
B. Both critics declared the required model/effort but reported no independently
visible runtime metadata. The packet-level model-truth gate therefore remains
**RED**.

## Terminal verdict

**REJECT. R3 remains RED. Clean count: zero. Regeneration is not authorized.**

Both hostile reviews are materially correct. The source receipt contains
semantic counterexamples, omits controlling source from custody, concedes a
stale non-compiling generated fleet, and does not implement BB-C.W0. It earns
**zero BB-C.W0, lock, wave, tranche, prototype, parity, performance,
convergence, regeneration, or movement credit**. Correct narrow repairs remain
incident-prerequisite evidence only.

This adjudication supersedes the R2 disposition for this R3 submission without
editing or accepting any source, generated, package, lock, stage, or Value
artifact.

## Reproduced custody

Required artifacts matched exactly:

| Artifact | SHA-256 |
|---|---|
| Binding R2 adjudication | `a4c51fd431913c63fa6a30e02f7a110653f6bf5c648f007c144f93acaad53ea7` |
| R3 subject | `8b23cef857e87f8416ef2b787d24876a6eaab9d8e53982cd5cef0b41d7237039` |
| R3 review A | `fdc45214a1d4c72144dca51dfe348ffd6e500483d05b00523ec9ccd97133e254` |
| R3 review B | `6c3604f851cae16039e8956ed22a0a533a0b7ba5dfa76365fa4d5a32659dd049` |

The subject's exact ledger independently reproduced:

- members: `103`;
- bytes: `1204031`;
- ordered newline-terminated path digest:
  `56a897ab80df83d10f4583db3008ddd726d389abd28a0b39e2a8f40e6adcde76`;
- ordered actual-`shasum -a 256` ledger digest:
  `7525d13a91522ee4dc8deb602965f1c02ca4168983d7cf6b09fca7a4cc4ff090`;
- member mismatches: `0`;
- bytes explicitly streamed through `od`: `1204031`;
- ledger-scoped `git diff --check`: PASS, zero output;
- ledger-scoped `push_branch_tag` occurrences: `0`.

The seal is exact but incomplete. At minimum these controlling files are absent:

- `crates/core/src/backend/rust/emitter/shapes/scalar.rs`;
- `crates/core/src/backend/util.rs`;
- `crates/core/src/backend/rust/emitter/regex_scan_adapter.rs`;
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs`;
- `crates/core/src/backend/rust/emitter/shapes/mod.rs`;
- `crates/core/src/backend/rust/emitter/shapes/dispatcher/ref_call.rs`;
- `crates/core/src/runtime/handle.rs`.

The frozen CSS Syntax and Color 4 objects also reproduced their content-address
hashes, respectively
`3f129d17407f9bd027291bcfb8015beb86fc229bf5efe505b49ec2dc30be4390`
and
`f5b6afc82d9198d130507448837e19c1b984ae9a1bebb579547822339aaf482a`.

No Cargo, regeneration, x86, source, generated, package, lock, stage, commit, or
Value mutation was run. The subject's reported Cargo/rustfmt results have no
immutable logs bound to the receipt and remain **UNPROVEN**. Its exact
`886`-error count is likewise **UNPROVEN**; the admitted product/check-runtime
RED is sufficient and is not disputed.

## Critic-finding adjudication

| Finding | Disposition | Reproduced basis |
|---|---|---|
| A payload fast-path defects | **CORRECT** | `branches.rs:775-909` admits every Regex but emits a hard-coded non-empty ASCII word scan and strips `OptionalWhitespace`; `flat/struct_direct.rs:244-260` admits any nonzero `Many` for multi-field structs; Inline rejects and Flat delegates `TokenDispatch` (`inline/structural_branch.rs:380-386`; `flat/struct_direct.rs:742-749`). |
| A CSS normative/host defects | **CORRECT** | Literal matching is case-sensitive; the grammar omits system colors, hue angles, and `none`; `parse_hex_color` calls `.expect` (`host.rs:17-21`) and HRegex invokes it as infallible `u32` (`hregex.rs:361-380`). |
| A direct-root-only numeric typing and TS escape defect | **CORRECT** | Literal parsing erases suffixes into `IntLit`/`UIntLit` (`literals.rs:6-68`); `MapExpr` has no domain carrier; `direct_numeric_suffix` inspects only the whole RHS (`wrap.rs:290-337`). `ts_escape` emits NUL as `\\0`, so NUL followed by a digit forms strict-invalid JavaScript such as `"\\01"`. |
| A incomplete custody | **CORRECT** | The seven controlling files listed above are outside the exact seal. |
| A Tape overclaim | **CORRECT** | Storage/read/rollback repairs are real, but `end_compound` ignores its handle and blindly pops (`tape/mod.rs:223-251`); bounds-end also blindly pops (`:195-203`). |
| B1 composite suffix loss/backend divergence | **CORRECT** | A folded composite becomes an untyped literal. TypeScript therefore selects `Number` absent a retained I64/U64 return type (`ts/projection.rs:92-124`), while Wasm applies the untyped i32 gate (`wasm/emitter/value.rs:43-102,145-187`). |
| B2 final-backslash CSS contradiction | **CORRECT** | The pinned Syntax snapshot states at `:4006-4007` that backslash followed by EOF is not a valid escape. `color.bbnf:192-202` and `runtime_projection_semantics.rs:341-405` encode the opposite boundary for `#abc\\`. |
| B3 CSS case/panic defects | **CORRECT** | The pinned Color 4 snapshot requires ASCII-case-insensitive named colors (`:1561-1568`); generated literal byte comparisons are exact (`branches.rs:577-594`), and the mapped converter can panic as above. |
| B4 duplicate Sheets operator authority | **CORRECT** | Only `ComparisonExpr` uses generated Pratt metadata (`regen_simple_runtime.rs:1276-1327`); `tag_lexeme` separately hard-codes the operator spellings (`:1524-1543`). |
| B5 classified-reference invariant | **CORRECT** | The solver assigns one payload to every classified non-Scalar reference (`branches.rs:251-305`), while `shapes/mod.rs:157-201` skips most transparent classified callees and `dispatcher/ref_call.rs:39-108` still composes their function names. |
| B6 Tape handle identity/order | **CORRECT** | `CompoundHandle` has public fields and constructor (`runtime/handle.rs:46-58,118-126`), while Tape ignores the supplied token. Forged, stale, duplicate, and out-of-order handles do not fail closed. |
| B7 absent product closure | **CORRECT**, exact count **UNPROVEN** | The subject concedes stale generated bytes and RED `check-runtime`; no governed generated parser proves these source contracts. The prose-only number `886` was not rerun. |

No material A/B finding is disproved.

## Subject-claim dispositions

| Claim | Disposition |
|---|---|
| 1. Literal exactness | **INCORRECT overall.** Direct signed minima, overflow checks, direct suffix projection, f32-to-f64 rounding, finite folds, and direct I64/U64 backend handling are correct narrow facts. Width/domain is not compositional across parentheses or arithmetic; TS/Wasm diverge, and TS StringLit escaping has the NUL-plus-digit defect. |
| 2. Path-sensitive payload formation | **INCORRECT/UNPROVEN overall.** Constant/StringLit replacement and supported Map, Skip, and Next rollback are correct narrowly. Ref availability, Regex semantics, OptionalWhitespace, exact aggregate arity, Scalar custody, Keyword/Pratt maps, and TokenDispatch are not closed. Repeat/Alt/recursion cardinality code is qualitative, not a path-total emitted-layout proof. |
| 3. Tape safety and identity | **INCORRECT/UNPROVEN overall.** The enumerated record size, checked reads, owned storage, identity widths, sentinel distinction, and rollback facts are correct. Handle/LIFO and bounds pairing are unenforced. Allocation, cloning, source-copy, and side-lane costs have no optimality credit. |
| 4. Fail-closed generators and serializer authority | **INCORRECT overall.** Simple scalar rejection and its unreachable builder arms are correct source repairs; the BBNF metadata path is present. Sheets retains a second operator table, and generated behavior is unproved. |
| 5. CSS Level 4 boundary | **INCORRECT overall.** Role separation, absent ColorMix/HueMethod ownership, percentage deposition, configured units, and 3/4/6/8 widths are correct narrow facts. EOF escape handling, ASCII case-insensitivity, system colors, hue/`none` syntax, and fallible no-panic conversion are wrong or absent. |
| 6. Integration custody | **INCORRECT overall.** The TS emitter is sealed and the 103 governed files contain zero `push_branch_tag`. The integration boundary omits controlling sources and intentionally excludes the stale generated fleet. |

## Wave fit and credit

BB-C.W0 requires G3 deletion, typed optimizer failures, ambient egraph/CSP
removal, a complete optimizer call-site inventory, and inverting mutations
(`waves/BB-C/W0.md:8-28`). This packet does none of them. Generator ownership is
BB-C.W5; Tape/value/CSS runtime closure is BB-D.W0/W1. Under the portfolio
supersession and complete-candidate laws, this cross-wave partial assay cannot
be transposed or credited as a product candidate.

## Binding R4 order

1. **Repair the five source authorities before regeneration.** Preserve numeric
   domain in the IR through composite folding/backends and make TS strings
   lexical-safe; make one emitted-path-aware exact payload/layout planner
   preserve real Regex and whitespace semantics and fail closed for unsupported
   Keyword/Pratt/TokenDispatch paths; make Tape handles opaque and enforce
   LIFO/bounds pairing; derive every Sheets operator spelling from generated
   metadata; implement pinned CSS token boundaries, ASCII case-insensitivity,
   complete claimed Color 4 syntax, and typed fallible host conversion.
2. **Prove those repairs with counterexamples and mutations.** Cover direct,
   parenthesized, composite, mixed-domain Rust/TS/Wasm literals; NUL-plus-digit
   strings; Ref/Scalar/Map/Skip/Next/Repeat/Alt/recursion/TokenDispatch payloads
   and exact arity; forged/stale/duplicate/out-of-order Tape handles; mutated
   Sheets metadata; CSS EOF escape, mixed case, system colors, hue/`none`, and
   no-panic malformed inputs.
3. **Seal the complete source boundary.** Include every changed source/test,
   the seven omitted controllers above, projection/serializer consumers, and
   the frozen normative objects. Publish members, bytes, ordered path digest,
   actual-hash ledger digest, zero mismatches, commands, and immutable results.
4. **Obtain two fresh hostile reviews and this same third-party adjudication
   shape.** Each reviewer must have independently visible model/effort metadata;
   any correct finding resets the clean count. Source acceptance still earns no
   BB-C.W0 or product credit.
5. **Only after an accepted R4 source receipt**, authorize one isolated ordered
   grammar-to-sidecar-to-runtime generation followed by a second empty/clean
   byte-identical pass. Generated files may change only through the generator.
6. **Then close product gates on native Apple ARM:** full Rust/TS/Wasm compile
   and differential execution, serializer parse/serialize/reparse, frozen CSS
   corpus and malformed-input no-panic gates, Tape misuse gates, deterministic
   artifacts, `check-runtime`, and product-wide zero legacy occurrences. Product
   acceptance still requires two consecutive clean hostile gestalts.

Until every ordered gate closes: **no regeneration now, no clean pass, no
movement, and zero credit**.
