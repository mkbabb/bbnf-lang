# W4 — Semantic proof, scalar/NEON profile, and cutover

Purpose: decide what is actually fast, transpose only proved organs, and delete the
replaced system.

## Scanner prototype

Using the W2 scalar oracle, derive one optional region-aware byte classifier:

- byte-class masks;
- finite lexical modes;
- quote and escape carry across blocks;
- excluded-region masks;
- delimiter candidate masks;
- explicit ineligibility/fallback report.

Instantiate it for strict JSON and CSS lexical regions without either backend
knowing the language name.

Implement stable AArch64 `std::arch` intrinsics only if the consuming W2 profile
meets its predeclared activation threshold. There is one selection point per
parser, not per block. If the threshold is not met, retain the scalar oracle and
record NEON as rejected for this vertical.

## Checkasm-style proof

- randomized scalar/NEON differential;
- carry-state comparison;
- every tail and alignment;
- guarded page boundaries;
- adversarial escapes/comments/UTF-8;
- sanitizer/Miri-compatible scalar boundary proof;
- isolated kernel and consuming parser timing;
- compiler assembly inspection.

Unsupported grammar modes deterministically select the exact W2 scalar event
implementation. There is no ambiguous second “general” runtime and no
format-specific patch.

## Competitive proof

Run the benchmark constitution on the equivalent full-consumption event plane.
CSS event/token claims compare to cssparser only after an adapter proves the same
consumption and checksum promise. JSON competitors remain candidates until an
adapter proves equivalent retained work. No DOM, selective, typed, or stylesheet
claim is available from this tranche.

Hand ASM is a conditional sub-row, not a deliverable. It activates only under the
charter's four admission conditions.

## Cutover

Before deletion, emit `artifacts/W4-REPLACEMENT.tsv` with one row per candidate:
old symbol/path, consumers, equivalent new symbol/path, crater command, proof
artifact, and disposition.

On accepted semantic/profile evidence:

- transpose only proved skinny kernels named in that matrix;
- delete only duplicate scanner/runtime/template paths proved replaced;
- remove repository-owned x86 production/build/CI/generated surfaces only where
  the selected vertical's replacement matrix supplies coverage;
- retire the whole skinny plane only when every supported public behavior is
  covered or explicitly rejected;
- do not leave aliases, shims, compatibility exports, or masked fallbacks.

Historical artifacts and unreconciled worktrees receive individual dispositions;
they are not bulk-deleted.

The selected vertical closes with zero x86 code or dispatch in its own production,
build, CI, and generated surfaces. Repo-wide x86 retirement is a separately
banked inventory: each remaining surface needs a consumer list, replacement
trigger, and evidence row before deletion. SK-V26 may not claim repo-wide
abrogation from one vertical.

## Exit

- scalar/NEON equivalence if NEON activated, otherwise a receipted rejection;
- semantic-plane conformance;
- honest raw profile packet and promote/bank/reject decision;
- one bounded path-level clean-break cutover;
- clean build/test/regen/package replay;
- tri-fold hostile cutover and performance review;
- every chronic/deferred row terminally built, banked with a measurable trigger,
  or retired.

Named evidence:

- `artifacts/W4-CHECKASM.json`;
- `artifacts/W4-PROFILE.json`;
- `artifacts/W4-REPLACEMENT.tsv`;
- `artifacts/W4-CRATER.tsv`;
- `artifacts/W4-REPLAY.md`.
