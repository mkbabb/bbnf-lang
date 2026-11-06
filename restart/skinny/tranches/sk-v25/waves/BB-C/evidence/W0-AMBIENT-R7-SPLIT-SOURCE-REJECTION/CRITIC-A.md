MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: hostile source critic A.

Model and effort are self-reported and independently unverified.

# Verdict: REJECT

Date: 2026-07-22. Isolated regeneration is not authorized.

Custody passed: the source receipt hashes to
`f6b2493c5ad479d1bd08faec62be7a2070fc95e09c80729037cd2bffda7851d5`;
all 36 members, 697,249 bytes, path ledger and ordered hash ledger reproduce.
The predecessor rejection hashes to
`f58781ad6867b75aaef4c9febb19e85d2086bac078f53dc75611b2568b9741cb`.

## Correct claims

- `AltBranchId(u16)` is nominal, exact, and checked at its inward public
  conversions.
- The principal emitter paths distinguish typed semantic U8 from structural
  Alt selection, and Pratt emits one U8 child per operator.
- Tape remains 16 bytes/aligned four and snapshots full Tape scope/span stacks.
- Sheets and simple-runtime U8 values remain explicit; JSON's current zero map
  remains Null.

## Blocking defects

1. Both compiler width gates traverse only `rule.body`. A recovery Alt under
   `rule.meta.directives.recover` bypasses them and is compiled later. Validate
   both roots and test exact maximum/first unsupported recovery counts.
2. HRegex has no input-derived U64 arm; U64 falls through to string emission.
   Its U8 input decoder also lacks the suffix handling used by wider inputs.
   One shared exact typed-integer decoder is required.
3. Mapped delegated Ref and mixed Seq branches in AltDispatch and Wrap can omit
   the outer Map payload. Preserve the peeled descriptor and emit it once after
   a successful delegated parse.
4. Tape's raw fields and constructors remain public and cursor typed readers are
   unchecked or debug-only. Seal raw mutation and make typed readers validate
   both Leaf kind and exact LeafType in release builds.
5. Generated Pratt is flat `[lhs, Tag, rhs, ...]`, but BBNF lowering recognizes
   only Span operators and the deposed three-child structural-branch reducer.
   Consume semantic Tag leaves through authoritative operator metadata and
   remove the structural reducer contract.
6. CSS routes `colorFn` to a `ColorFunction` frame and stores its first U8
   `colorSpace` value in `kind_tag`; closure therefore constructs a functional
   color instead of `CssColor::Predefined`. Use distinct functional/predefined
   frame roles. The projection TOML is part of the repaired subject.
7. Generated wide values silently narrow: Sheets U64 to F64, BBNF U64 to I64,
   and CSS wide U64 to F64. Add exact storage or reject unsupported projections
   at generation time.
8. Non-generated external tests still call the removed `push_branch_tag` API:
   `sheets_parity`, `sheets_expr_parity`, `google_sheets_slab`,
   `css_l4_substrate`, `bbnf_struct_builder_substrate`, and
   `json_parity_struct`. Migrate semantic uses to `push_leaf_with_u8` and
   structural uses to `select_alt_branch(AltBranchId)`; add no shim.
9. JSON's generator assumes every U8 means Null. Validate at generation time
   that JSON's U8 contract contains only exact zero.

The repaired receipt needs tests for recovery width, HRegex typed integers,
mapped delegated branches, generated repeated BBNF Pratt lowering, CSS
predefined/function separation, exact wide values, JSON's zero-only contract,
and release cursor misuse. No files were modified and no regeneration ran.
