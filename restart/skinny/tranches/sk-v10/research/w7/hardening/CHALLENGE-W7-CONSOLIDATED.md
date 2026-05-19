# SK-V10 W7 CHALLENGE - Consolidated

Disposition: ACCEPT.

## CH1 Correctness

ACCEPT. The selected primitive family is exactly `C5-full-string-proof`, bound
to the current trusted UTF-8 JSON string caller:
`match_string_at_quote_trusted_utf8` through `skip_string_plain_trusted`.
The scalar oracle is already in the primitive owner path as
`scan_string_special_block_scalar`, and the caller scalar mirror is the current
8-byte SWAR `string_special_mask` semantics. That is a valid correctness
surface for a proof-only micro wave.

Redress must not infer row movement from this proof. The only admissible W7
claim is caller-level isolated headroom versus a scalar-only mirror.

## CH2 Generality / Lock 14

ACCEPT. The plan does not edit generic codegen behavior, runtime JSON policy,
or generated typed/direct parsers. The read-only generated owner paths are
call-site evidence only. If redress edits only the primitive/proof artifact
paths and `parse-that-regex` only for a behavior-free harness, no new Lock 14
allowance is required.

The proof remains JSON-scoped. It cannot be generalized to CSS L4, Sheets, or
BBNF-self without a later Section 2.1 proof because JSON string policy is
stricter around slash, escape, and control-byte handling.

## CH3 Regression / REDRESS

ACCEPT. W7 has no production wiring and no `RESULTS.md` row movement, so the
revert surface is limited to proof/harness/microbench artifacts. If parity or
microbench fails, redress records the observed failure and preserves the W7 plan
as a rejected proof route.

W9 remains responsible for any production caller regression, direct/typed row
floors, Track 2/oracle independence, and W10b maintain floors.

## CH4 Cost And Micro-Proof Adequacy

ACCEPT. The predeclared threshold is caller-level `>=1.08x` over a scalar-only
mirror on representative corpus slices. A primitive-only nanobench is not
sufficient. The artifact must record:

- observed aggregate speedup;
- threshold;
- run id;
- host triple;
- build flags;
- feature gate;
- representative corpus slices;
- sample count;
- scalar oracle identity;
- differential harness identity;
- current caller identity.

If the aggregate speedup is below `1.08x`, W7 fails closed even if isolated
primitive parity is green.

## CH5 Hidden Coupling

ACCEPT WITH WATCHPOINTS. `skip_string_plain_trusted` is already production
wired on aarch64. The proof must therefore avoid landing behavior changes under
the guise of measurement. A scalar-only mirror may live in the proof artifact
or in a test-only/harness-only path, but redress must not change the production
branch, target-feature guard, string semantics, or generated call sites.

The representative slices must include `unicode_mixed`, `unicode_escapes`, and
`unicode_basic`. ASCII-only toy strings are not sufficient because they do not
exercise the unicode-row loss shape W9 would later target.

## CH6 Anti-Paper-Close

ACCEPT. The plan blocks the paper-close route by requiring both differential
parity and a threshold-bearing caller microbench. The phrase "already wired" is
not evidence. W7 closes only if the artifact shows the current caller beating a
scalar-only mirror by the predeclared threshold on same-host representative
slices.

## Required Redress Discipline

Redress may proceed without a plan revision. It must:

1. Keep W7 proof-only and behavior-free.
2. Run the scalar/reference parity tests and checkasm differential surface.
3. Produce the caller microbench artifact under
   `restart/skinny/tranches/sk-v10/research/p3/string-primitive-proof/`.
4. Record PASS only if the aggregate observed speedup is `>=1.08x`.
5. Leave `skinny/RESULTS.md` unchanged.
