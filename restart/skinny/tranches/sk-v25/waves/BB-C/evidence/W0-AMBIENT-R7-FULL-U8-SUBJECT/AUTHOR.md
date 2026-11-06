MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: R7 full-u8 transport repair author.

# BB-C.W0 R7 full-u8 Pratt/tape subject

- Date: 2026-07-22.
- Workspace: `/Users/mkbabb/Programming/bbnf-lang`.
- Status: **CANDIDATE FOR TWO HOSTILE REVIEWS**. No regeneration, fleet,
  runtime projection, landing, stage, commit, package change, parse-that
  mutation, or Value contact is authorized by this receipt.
- Predecessor: the third R3 adjudicator accepted the narrow mapped-operator
  Pratt repair but proved the packed tape's global `>63` truncation contract
  incorrect. This subject closes that carried defect before the atomic fleet.

## Exact subject

| Path | Bytes | SHA-256 |
|---|---:|---|
| `crates/ir/src/registry/mod.rs` | 2,202 | `dade11d975f3b1f4e35ff9867d44fd70d5db7ea0e6b1990d6aca7bd8e045903e` |
| `crates/ir/src/lib.rs` | 2,073 | `cdf6a1689f8fb02c6dab0527cb15565b44c9994cddecc73b4444ebaec2ca33ac` |
| `crates/ir/src/passes/recognizers/grammar_facts.rs` | 66,226 | `9770a392d6f91619019f7235950b1fbd1542ba23cb59c4170d9901df92132474` |
| `crates/core/src/runtime/tape/record.rs` | 7,426 | `7c1ff243a55fc5fb0bbd3efc79f6bd9ea44a697e5df6bf14b19307bf96ab5185` |
| `crates/core/src/runtime/tape/mod.rs` | 9,966 | `3e6103ceab885ed79b7edba1e3258e9560c5eea3b2562c9e1337c730cbcdbe99` |
| `crates/core/tests/pratt_const_fold.rs` | 21,424 | `73c745c4d32751371cf503520158c98ee8752aedd3fcf87bf558660fc016f337` |
| `crates/core/tests/tape_substrate.rs` | 6,678 | `6589c672cb361621aeee0baa6b76b9b0750ae47d9abec9f0d1a050a483973c3b` |
| `crates/ir/tests/shape_dispatch.rs` | 48,058 | `328090894049a498fefaf2d7ea421aa407d3df5bf9d88a85ad36d93653474477` |

The eight members total 164,053 bytes. Reviewers must reject the receipt if
any byte identity differs.

## Repair claim

`TapeRec` remains `#[repr(C, align(4))]`, 16 bytes, and four records still fit
one 64-byte cache line. Compound records now store their full `u8` branch tag
in the low byte of the already-present `extra: u16`; that field remains the
leaf-type/string-borrow carrier for leaf records. The two live `flags` bits
continue to carry only `has_children` and the high `meta_idx` bit. No new field,
allocation, compatibility path, or grammar-specific branch was added.

`TapeStructBuilder::push_branch_tag(u32)` now performs a checked conversion to
`u8` and fails closed on an internal caller outside the representable domain;
it no longer masks or truncates. `BRANCH_TAG_MAX` is consequently `u8::MAX`,
so an exact typed-u8 operator map may use every value in its declared type.

External tests prove both the direct record and builder paths preserve 255,
the record remains 16 bytes/aligned 4, and a Pratt operator mapped to `255u8`
retains that discriminant. Existing negative proofs still reject non-u8,
mixed mapped/bare, and three-byte operator forms.

## Native Apple ARM evidence

Host and target were measured as `arm64` / `aarch64-apple-darwin`. Fresh
commands on the exact subject returned:

| Command | Result |
|---|---|
| `cargo test -p bbnf --test pratt_const_fold` | 21 passed / 0 failed |
| `cargo test -p bbnf-ir --test shape_dispatch` | 39 passed / 0 failed |
| `cargo test -p bbnf --test tape_substrate` | 11 passed / 0 failed |
| slice-local `rustfmt` plus `git diff --check` | GREEN |

The compiler emitted pre-existing workspace warnings; none is a test failure
or evidence of x86 execution. The author did not run a generated/runtime
product proof. That remains forbidden until two hostile reviewers and a third
adjudicator accept this exact subject.

The model declaration above is self-reported metadata, not independent
attestation, and earns no MODEL-TRUTH credit.
